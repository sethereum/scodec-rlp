package com.github.sethereum.rlp

import scodec.Attempt.{Failure, Successful}
import scodec.bits.{ByteVector, BitVector}
import scodec.codecs._
import scodec.{Err, DecodeResult, Attempt, SizeBound}

/**
 * Encodes/decodes a byte sequence using RLP encoding.
 *
 * @see http://gavwood.com/paper.pdf - Appendix B
 */
object RlpBytesCodec extends RlpCodec[ByteVector] {

  override def sizeBound: SizeBound = SizeBound.unknown

  override def encode(value: ByteVector): Attempt[BitVector] = {
    if ((value.length == 1) && (value(0) > 0) && (value(0) < 128)) {
      Successful(value.bits)
    } else {
      for (len <- RlpBytesLengthCodec.encode(value.length))
        yield len ++ value.bits
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[ByteVector]] = {
    for {
      header <- uint8.decode(bits)
      data <-
        if (header.value < 128) {
          Successful(header.map(x => ByteVector(x)))
        } else for {
          len <- RlpBytesLengthCodec.decode(bits)
          data <- bytes(len.value.toInt).decode(len.remainder)
        } yield data
    } yield data
  }

}

class RlpFixedSizeBytesCodec(val bytes: Int) extends RlpCodec[ByteVector] {

  override def sizeBound: SizeBound = SizeBound.exact(bytes * 8)

  override def encode(value: ByteVector): Attempt[BitVector] = {
    if (value.length != bytes) {
      Failure(Err(s"unexpected bytes size (expected: $bytes, actual: ${value.length}"))
    } else {
      RlpBytesCodec.encode(value)
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[ByteVector]] = {
    for {
      value <- RlpBytesCodec.decode(bits)
      validated <-
        if (value.value.length != bytes) {
          Failure(Err(s"unexpected bytes size (expected: $bytes, actual: ${value.value.length}"))
        } else {
          Successful(value)
        }
    } yield validated
  }

}