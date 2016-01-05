package com.github.sethereum.rlp

import scodec.Attempt.{Successful, Failure}
import scodec.bits.BitVector
import scodec._
import scodec.codecs._

class RlpBigIntCodec(val bits: Int) extends RlpCodec[BigInt] {
  require(bits > 0, s"bits must be positive")

  override def sizeBound: SizeBound = SizeBound.bounded(8, bits)

  override def encode(value: BigInt): Attempt[BitVector] = {
    value.signum match {
      case -1 => Failure(Err(s"encoding negative scalar values is not supported (value: $value)"))
      case 0 => uint8.encode(0)
      case 1 => {
        if (value < 128) {
          uint8.encode(value.intValue())
        } else {
          // Drop leading zero byte, if necessary (see Appendix B.165)
          val b = value.toByteArray
          val p = if (b(0) == 0) b.drop(1) else b
          RlpBytesCodec.encode(p)
        }
      }
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BigInt]] = {
    RlpBytesCodec.decode(bits).flatMap { result =>
      if (result.value.length == 0) {
        Failure(Err(s"error decoding zero length scalar"))
      } else if (result.value.length > 1 && result.value(0) == 0) {
        // Leading zeros are disallowed (see Appendix B.165)
        Failure(Err(s"error decoding scalar with leading zeros (value: ${result.value})"))
      } else {
        Successful(result.map(b => BigInt(1, b)))
      }
    }
  }

}
