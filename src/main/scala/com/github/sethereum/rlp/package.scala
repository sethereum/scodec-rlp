package com.github.sethereum

import java.lang.{Integer => javaInt, Long => javaLong}
import java.nio.charset.Charset

import scodec._
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless._

import scala.language.implicitConversions

/**
 * Codec support for RLP encoding, as defined by the Ethereum project.
 *
 * See: https://github.com/ethereum/wiki/wiki/RLP
 * See: http://gavwood.com/paper.pdf (Appendix B)
 */
package object rlp {


  // Basic data type codecs

  val rlpBytes = RlpCodec(RlpBytesCodec.xmap[Array[Byte]](_.toArray, ByteVector.apply))
  def rlpBytes(bytes: Int) = new RlpFixedSizeBytesCodec(bytes).xmap[Array[Byte]](_.toArray, ByteVector.apply)

  def rlpShort(bits: Int) = new RlpShortCodec(bits)
  def rlpInt(bits: Int) = new RlpIntCodec(bits)
  def rlpLong(bits: Int) = new RlpLongCodec(bits)
  def rlpBigInt(bits: Int) = new RlpBigIntCodec(bits)

  def rlpString(implicit charset: Charset): RlpCodec[String] = RlpCodec(RlpBytesCodec.narrow[String](
    s => string.decode(s.bits).map(_.value),
    s => ByteVector(s.getBytes(charset)))
  )

  // List codecs

  implicit def rlpList[A](implicit itemCodec: RlpCodec[A]) = new RlpListCodec[A](itemCodec)

  implicit def rlpHList[H <: HList](codec: Codec[H]): RlpCodec[H] = new RlpHListCodec[H](codec)

  // Ethereum-specific data type definitions

  val rlpP = rlpBigInt(1000)
  val rlpP5 = rlpInt(5)
  val rlpP256 = rlpBigInt(256)

  val rlpB8 = rlpBytes(8)
  val rlpB20 = rlpBytes(20)
  val rlpB32 = rlpBytes(32)
  val rlpB160 = rlpBytes(160)
  val rlpB256 = rlpBytes(256)

  // Utility functions

  def leftTrimmedBytesLength(value: Int) = Math.max(javaInt.SIZE - javaInt.numberOfLeadingZeros(value), 8) / 8
  def leftTrimmedBytesLength(value: Long) = Math.max(javaLong.SIZE - javaLong.numberOfLeadingZeros(value), 8) / 8

}
