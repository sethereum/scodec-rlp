package com.github.sethereum.rlp

import scodec.Attempt.{Failure, Successful}
import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Err, DecodeResult, Attempt, SizeBound}

object RlpBytesCodec extends RlpCodec[Array[Byte]] {

  override def sizeBound: SizeBound = SizeBound.unknown

  override def encode(value: Array[Byte]): Attempt[BitVector] = {
    if (value.length == 1 && value(0) < 128) {
      Successful(BitVector(value))
    } else if (value.length < 56) {
      for {
        len <- uint8.encode(128 + value.length)
        data <- Successful(BitVector(value))
      } yield len ++ data
    } else {
      for {
        len <- LeftTrimmedLongCodec.encode(value.length)
        lenlen <- uint8.encode(183 + len.bytes.length)
        data <- Successful(BitVector(value))
      } yield lenlen ++ len ++ data
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[Array[Byte]]] = {
    for {
      header <- uint8.decode(bits)
      data <-
        if (header.value < 128) {
          Successful(header.map(h => Array(h.toByte)))
        } else if (header.value < (128 + 56)) {
          for {
            b <- bytes(header.value - 128).decode(header.remainder)
          } yield b.map(_.toArray)
        } else if (header.value < (183 + 8)) {
          for {
            len <- ulong(header.value - 183).decode(header.remainder)
            b <- bytes(len.value.toInt).decode(len.remainder)
          } yield b.map(_.toArray)
        } else {
          Attempt.failure[DecodeResult[Array[Byte]]](Err(s"invalid RLP byte array header ${header.value}"))
        }
    } yield data
  }
}
