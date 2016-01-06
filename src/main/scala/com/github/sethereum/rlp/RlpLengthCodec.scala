package com.github.sethereum.rlp

import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits.BitVector
import scodec.codecs._

/**
 * Encodes/decodes bytes or list length
 * 
 * @param offset length value base
 */
private [rlp] sealed abstract class RlpLengthCodec(val offset: Int) extends Codec[Long]{

  override def sizeBound: SizeBound = SizeBound.bounded(1, 9) * 8

  override def encode(value: Long): Attempt[BitVector] = {
    if (value < 56) {
      uint8.encode(offset + value.toInt)
    } else {
      for {
        len <- ulong(leftTrimmedBytesLength(value) * 8).encode(value)
        header <- uint8.encode(offset + 56 - 1 + len.bytes.length)
      } yield header ++ len
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[Long]] = {
    for {
      header <- uint8.decode(bits)
      len <-
        if (header.value < offset) {
          Failure(Err(s"invalid RLP length header ${header.value} for base $offset"))
        } else if (header.value < (offset + 56)) {
          Successful(header.map(_.toLong - offset))
        } else {
          ulong((header.value - offset - 56 + 1) * 8).decode(header.remainder)
        }
    } yield len
  }

}

private [rlp] object RlpBytesLengthCodec extends RlpLengthCodec(128)
private [rlp] object RlpListLengthCodec extends RlpLengthCodec(192)
