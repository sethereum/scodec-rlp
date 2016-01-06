package com.github.sethereum.rlp

import java.lang.{Integer => javaInt, Long => javaLong, Short => javaShort}

import com.github.sethereum.rlp
import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

/**
 * Trims leading zeros during encoding.
 *
 * Note that the standard scodec variable length codecs (vlong and vint) will NOT work.
 *
 * Must be wrapped with a variable length codec (since the resulting bit vector will be value dependent).
 */
private [rlp] object LeftTrimmedShortCodec extends Codec[Short] {

  override def sizeBound: SizeBound = SizeBound.bounded(8, javaShort.SIZE)

  override def encode(value: Short): Attempt[BitVector] =
    Attempt.successful(BitVector.fromShort(value, leftTrimmedBytesLength(value) * 8))

  override def decode(bits: BitVector): Attempt[DecodeResult[Short]] = ushort(bits.length.toInt).decode(bits)
}

/**
 * Trims leading zeros during encoding.
 *
 * Note that the standard scodec variable length codecs (vlong and vint) will NOT work.
 *
 * Must be wrapped with a variable length codec (since the resulting bit vector will be value dependent).
 */
private [rlp] object LeftTrimmedIntCodec extends Codec[Int] {

  override def sizeBound: SizeBound = SizeBound.bounded(8, javaInt.SIZE)

  override def encode(value: Int): Attempt[BitVector] =
    Attempt.successful(BitVector.fromInt(value, leftTrimmedBytesLength(value) * 8))

  override def decode(bits: BitVector): Attempt[DecodeResult[Int]] = uint(bits.length.toInt).decode(bits)
}

/**
 * Trims leading zeros during encoding.
 *
 * Note that the standard scodec variable length codecs (vlong and vint) will NOT work.
 *
 * Must be wrapped with a variable length codec (since the resulting bit vector will be value dependent).
 */
private [rlp] object LeftTrimmedLongCodec extends Codec[Long] {

  override def sizeBound: SizeBound = SizeBound.bounded(8, javaLong.SIZE)

  override def encode(value: Long): Attempt[BitVector] =
    Attempt.successful(BitVector.fromLong(value, leftTrimmedBytesLength(value) * 8))

  override def decode(bits: BitVector): Attempt[DecodeResult[Long]] = ulong(bits.length.toInt).decode(bits)
}
