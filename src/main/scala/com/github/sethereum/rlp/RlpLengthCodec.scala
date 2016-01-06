package com.github.sethereum.rlp

import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector
import scodec._
import scodec.codecs._

/**
 * Encodes/decodes bytes or list length
 * 
 * @param base length value base
 */
private [rlp] sealed abstract class RlpLengthCodec(val base: Int) extends Codec[Long]{

  override def sizeBound: SizeBound = SizeBound.bounded(1, 9) * 8

  override def encode(value: Long): Attempt[BitVector] = {
    if (value < 56) {
      uint8.encode(base + value.toInt)
    } else {
      for {
        len <- vlong.encode(value)
        header <- uint8.encode(base + 56 - 1 + len.bytes.length)
      } yield header ++ len
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[Long]] = {
    for {
      header <- uint8.decode(bits)
      len <-
        if (header.value < base) {
          Failure(Err(s"invalid RLP length header ${header.value} for base $base"))
        } else if (header.value < (base + 56)) {
          Successful(header.map(_.toLong - base))
        } else {
          ulong((header.value - base - 56 + 1) * 8).decode(header.remainder)
        }
    } yield len
  }

}

private [rlp] object RlpBytesLengthCodec extends RlpLengthCodec(128)
private [rlp] object RlpListLengthCodec extends RlpLengthCodec(192)
