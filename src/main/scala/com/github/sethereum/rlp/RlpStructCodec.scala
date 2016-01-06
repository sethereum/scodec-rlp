package com.github.sethereum.rlp

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}


/**
 * Encodes/decodes a structure by treating it as a RLP list (length prefixes the structure).
 *
 * @param codec codec used to serialize the list
 */
class RlpStructCodec[A](val codec: Codec[A]) extends RlpCodec[A] {

  override def sizeBound: SizeBound = RlpListLengthCodec.sizeBound + codec.sizeBound

  override def encode(value: A): Attempt[BitVector] = {
    for {
      data <- codec.encode(value)
      len <- RlpListLengthCodec.encode(data.bytes.length)
    } yield len ++ data
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
    for {
      len <- RlpListLengthCodec.decode(bits)
      data <- bytes(len.value.toInt).decode(len.remainder)
      items <- codec.decode(data.value.bits)
    } yield items.mapRemainder(_ => data.remainder)
  }

}
