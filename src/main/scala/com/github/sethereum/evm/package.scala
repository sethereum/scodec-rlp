package com.github.sethereum

import java.time.{LocalDateTime, ZoneOffset}

import com.github.sethereum.rlp._
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.KeccakDigest

import scala.language.implicitConversions

package object evm {

  class EvmException(msg: String) extends Exception(msg) {
    def this() = this("")
  }

  type EvmStorage = Map[EvmStorage.Key, EvmStorage.Value]
  object EvmStorage {
    type Key = EvmWord
    type Value = EvmWord

    val empty = Map.empty[Key, Value].withDefaultValue(EvmWord.Zero)

    object Key {
      def apply(bytes: Array[Byte]): EvmStorage.Key = EvmWord(bytes)
    }
    object Value {
      def apply(bytes: Array[Byte]): EvmStorage.Value = EvmWord(bytes)
    }
  }

  // Generic data type definitions

  type B = Seq[Byte]
  val b = rbyteseq

  type B8 = Seq[Byte]
  val B8 = B(8)
  val b8 = rbyteseq(8)

  type B20 = Seq[Byte]
  val B20 = B(20)
  val b20 = rbyteseq(20)

  type B32 = Seq[Byte]
  val B32 = B(32)
  val b32 = rbyteseq(32)

  type B_32 = Seq[Byte]
  val B_32 = B(0, 32)
  val b_32 = rbyteseq(0, 32)

  type B256 = Seq[Byte]
  val B256 = B(256)
  val b256 = rbyteseq(256)

  type P = BigInt
  val p = rbigint

  type P5 = Int
  val P5 = P(5, (1 << 5) - 1)
  val p5 = ruint(5)

  type P256 = BigInt    // Positive integer < 2^256
  val P256 = P(256, (BigInt(1) << 256) - 1)
  val p256 = rbigint(256)


  // Evm type value classes

  case class EvmAddress(val value: B20) extends AnyVal
  object EvmAddress {
    val Zero = EvmAddress(Seq.fill(20)(0.toByte))
  }
  val evmAddress = rlpCodec(b20.xmap[EvmAddress](EvmAddress.apply, _.value))

  case class EvmHash(val value: B32) extends AnyVal
  object EvmHash {
    val Empty = keccak256(Seq.empty)

    def keccak256(bytes: Seq[Byte]): EvmHash = {
      val digest: Digest = new KeccakDigest(256)
      val out = Array.ofDim[Byte](32)

      digest.update(bytes.toArray, 0, bytes.length)
      digest.doFinal(out, 0)
      EvmHash(out)
    }
  }
  val evmHash = rlpCodec(b32.xmap[EvmHash](EvmHash.apply, _.value))

  case class EvmTimestamp(val value: P256) extends AnyVal
  object EvmTimestamp {
    def now() = EvmTimestamp(LocalDateTime.now.toEpochSecond(ZoneOffset.UTC))
  }
  val evmTimestamp = rlpCodec(p256.xmap[EvmTimestamp](EvmTimestamp.apply, _.value))

  case class EvmNonce(val value: B8) extends AnyVal
  object EvmNonce {
    val Zero = EvmNonce(Seq.fill(8)(0.toByte))
  }
  val evmNonce = rlpCodec(b8.xmap[EvmNonce](EvmNonce.apply, _.value))

  case class EvmBalance(val value: P256) extends AnyVal
  object EvmBalance {
    val Zero = EvmBalance(0)
  }
  val evmBalance = rlpCodec(p256.xmap[EvmBalance](EvmBalance.apply, _.value))

  case class EvmValue(val value: P256) extends AnyVal
  object EvmValue {
    val Zero = EvmValue(0)
  }
  val evmValue = rlpCodec(p256.xmap[EvmValue](EvmValue.apply, _.value))

  case class EvmDifficulty(val value: P) extends AnyVal
  object EvmDifficulty {
    // Block header constraints (see section 4.3.4)
    val Minimum = EvmDifficulty(131072)
  }
  val evmDifficulty = rlpCodec(p.xmap[EvmDifficulty](EvmDifficulty.apply, _.value))

  case class EvmBlockNum(val value: P) extends AnyVal
  object EvmBlockNum {
    val Zero = EvmBlockNum(0)
  }
  val evmBlockNum = rlpCodec(p.xmap[EvmBlockNum](EvmBlockNum.apply, _.value))

  case class EvmGas(val value: P) extends AnyVal
  object EvmGas {
    val Zero = EvmGas(0)
    // Block header constraints (see section 4.3.4)
    val MinimumLimit = EvmGas(5000)
  }
  val evmGas = rlpCodec(p.xmap[EvmGas](EvmGas.apply, _.value))

  case class EvmBloom(val value: B256) extends AnyVal
  object EvmBloom {
    val Empty = EvmBloom(Seq.fill(256)(0.toByte))
  }
  val evmBloom = rlpCodec(b256.xmap[EvmBloom](EvmBloom.apply, _.value))


  /**
   * Ethereum byte sequence initializer factory.
   *
   * The returned function checks the supplied byte array and throws IllegalArgumentException on size mismatch.
   *
   * @param bytes expected byte array size
   * @return a function that checks the size of the supplied byte array
   */
  def B(bytes: Int) = {
    (value: Seq[Byte]) => {
      require(value.length == bytes, s"byte array size mismatch (expected: $bytes, actual ${value.length})")
      value
    }
  }

  def B(min: Int, max: Int) = {
    (value: Seq[Byte]) => {
      require(value.length < min || value.length > max, s"byte array size mismatch (expected: [$min, $max], actual ${value.length})")
      value
    }
  }

  def P[A](bits: Int, limit: A)(implicit integral: Integral[A]): A => A = {
    import integral._
    (value: A) => {
      require(value >= zero && value <= limit, s"scalar value negative or out of range (bits: $bits, value: $value")
      value
    }
  }


}
