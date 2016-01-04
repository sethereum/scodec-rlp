package com.github.sethereum

import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.KeccakDigest

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

}
