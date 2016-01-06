package com.github.sethereum

import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.KeccakDigest
import rlp._

import scala.language.implicitConversions

package object evm {

  class EvmException(msg: String) extends Exception(msg) {
    def this() = this("")
  }

  type EvmStorage = Map[EvmStorage.Key, EvmStorage.Value]
  object EvmStorage {
    type Key = EvmWord
    type Value = EvmWord

    val empty = Map.empty[Key, Value].withDefaultValue(EvmWord.ZERO)

    object Key {
      def apply(bytes: Array[Byte]): EvmStorage.Key = EvmWord(bytes)
    }
    object Value {
      def apply(bytes: Array[Byte]): EvmStorage.Value = EvmWord(bytes)
    }
  }

  type EvmValue = BigInt
  type EvmTimestamp = Long

  // Basic data type definitions
  type P5 = Byte
  type P256 = BigInt    // Positive integer < 2^256
  type B32 = Seq[Byte]

  val P256UpperLimit = BigInt(1) << 256
  
  def requireP5(p5: P5) =
    require(p5 > 0 && p5 < (1 << 5), s"invalid P5 value: $p5")

  def requireP256(p256: P256) =
    require(p256 > 0 && p256 < P256UpperLimit, s"invalid P256 value: $p256")

  def requireB32(b32: B32) =
    require(b32.length == 32, s"invalid B32 length (expected: 32, actual: ${b32.length}")


  type EvmNonce = P256
  val requireNonce = requireP256 _

  type EvmBalance = P256
  val requireBalance = requireP256 _

  type EvmHash = B32
  val requireHash = requireB32 _


  // Block header constraints (see section 4.3.4)
  val MinimumDifficulty = 131072
  val MiniumGasLimit = 5000


  val EmptyKeccakDigest = keccakDigest(Seq.empty)

  // TODO: Move this elsewhere
  def keccakDigest(bytes: Seq[Byte]): Seq[Byte] = {
    val digest: Digest = new KeccakDigest(256)
    val out = Array.ofDim[Byte](256/8)

    digest.update(bytes.toArray[Byte], 0, bytes.length)
    digest.doFinal(out, 0)
    out
  }


  abstract class PositiveScalarCompanion[A : Integral, P <: AnyVal](val bits: Int) {
    val MaxValue: A
    def apply(i: A): P
    implicit val rlp: RlpCodec[P]
    implicit def pToIntegral(p: P): A
  }

  abstract class ByteSequenceCompanion[B](val size: Int) {
    def validate(b: Array[Byte]): Unit = {
      require(b.length == size, s"invalid byte array size (required: $size, actual: ${b.length})")
    }
    implicit val rlp: RlpCodec[B]
    implicit def bToBytes(b: B): Array[Byte]
    def apply(bytes: Array[Byte]): B
  }

  class P5a private (val i: Int) extends AnyVal

  object P5a extends PositiveScalarCompanion[Int, P5a](5) {
    val MaxValue = (1 << bits) - 1
    override implicit val rlp = RlpCodec(rlpInt(bits).xmap[P5a](P5a.apply, _.i))
    override implicit def pToIntegral(p5: P5a) = p5.i
    def apply(i: Int): P5a = {
      require(i <= MaxValue, s"value $i greater than maximum $MaxValue")
      new P5a(i)
    }
  }
  
  class P256a private (val i: BigInt) extends AnyVal

  object P256a extends PositiveScalarCompanion[BigInt, P256a](256) {
    val MaxValue = (BigInt(1) << bits) - 1
    override implicit val rlp = RlpCodec(rlpBigInt(bits).xmap[P256a](P256a.apply, _.i))
    override implicit def pToIntegral(p256: P256a) = p256.i
    def apply(i: BigInt): P256a = {
      require(i <= MaxValue, s"value $i greater than maximum $MaxValue")
      new P256a(i)
    }
  }

  class B256 private (val bytes: Array[Byte]) extends AnyVal

  object B256 extends ByteSequenceCompanion[B256](256) {
    override implicit val rlp: RlpCodec[B256] = RlpCodec(rlpBytes(size).xmap[B256](B256.apply, _.bytes))
    override implicit def bToBytes(b: B256): Array[Byte] = b.bytes
    override def apply(bytes: Array[Byte]): B256 = {
      validate(bytes)
      new B256(bytes)
    }
  }


  case class Sample(p256a: P256a, b256: B256)

  implicit val sample: RlpCodec[Sample] = rlpStruct((P256a.rlp :: B256.rlp).as[Sample])
}
