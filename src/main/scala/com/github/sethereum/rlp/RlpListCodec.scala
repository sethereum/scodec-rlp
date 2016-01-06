package com.github.sethereum.rlp

import scodec._
import scodec.bits.BitVector
import scodec.codecs._

class RlpListCodec[A](val itemCodec: RlpCodec[A]) extends RlpCodec[List[A]] {

  val listCodec = list(itemCodec)

  override def sizeBound: SizeBound = SizeBound.unknown

  override def encode(value: List[A]): Attempt[BitVector] = {
    for {
      data <- listCodec.encode(value)
      len <- RlpListLengthCodec.encode(data.bytes.length)
    } yield len ++ data
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[List[A]]] = {
    for {
      len <- RlpListLengthCodec.decode(bits)
      data <- bytes(len.value.toInt).decode(len.remainder)
      items <- listCodec.decode(data.value.bits)
    } yield items.mapRemainder(_ => data.remainder)
  }

}
