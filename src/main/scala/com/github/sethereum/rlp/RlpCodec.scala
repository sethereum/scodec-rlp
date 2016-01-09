package com.github.sethereum.rlp

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

/**
 * Marker trait used to indicate codecs that encode/decode data using RLP encoding.
 *
 * This trait is used to help enforce proper RLP codec construction in a type safe manner.
 * Use the RlpCodec companion object for wrapping over an existing codec (that encodes in RLP!).
 *
 * @tparam A
 */
sealed trait RlpCodec[A] extends Codec[A]

object RlpCodec {

  // Wraps a normal codec
  def apply[A](codec: Codec[A]): RlpCodec[A] = codec match {
    case c: RlpCodec[A] => c
    case c => new RlpCodec[A] {
      val codec = c
      override def sizeBound: SizeBound = codec.sizeBound
      override def decode(bits: BitVector): Attempt[DecodeResult[A]] = codec.decode(bits)
      override def encode(value: A): Attempt[BitVector] = codec.encode(value)
    }
  }

}
