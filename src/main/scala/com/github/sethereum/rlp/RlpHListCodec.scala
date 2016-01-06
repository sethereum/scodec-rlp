package com.github.sethereum.rlp

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{DecodeResult, Attempt, SizeBound, Codec}
import shapeless.HList


/**
 * Encodes/decodes a heterogenous list (HList).
 *
 * @param codec codec used to serialize the list
 */
class RlpHListCodec[H <: HList](codec: Codec[H]) extends RlpCodec[H] {

  override def sizeBound: SizeBound = RlpListLengthCodec.sizeBound + codec.sizeBound

  override def encode(value: H): Attempt[BitVector] = {
    for {
      data <- codec.encode(value)
      len <- RlpListLengthCodec.encode(data.bytes.length)
    } yield len ++ data
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[H]] = {
    for {
      len <- RlpListLengthCodec.decode(bits)
      data <- bytes(len.value.toInt).decode(len.remainder)
      items <- codec.decode(data.value.bits)
    } yield items.mapRemainder(_ => data.remainder)
  }

}
