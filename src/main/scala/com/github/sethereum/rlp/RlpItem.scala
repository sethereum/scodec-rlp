package com.github.sethereum.rlp

import scodec._
import scodec.bits.BitVector
import scodec.codecs._


// Recursive RLP item data structures
// NOT FOR EXTERNAL USE (see the supplied codecs in the rlp package object instead)

private [rlp] sealed trait RlpItem
private [rlp] case class RlpString[A] private[rlp] (bits: BitVector) extends RlpItem
private [rlp] case class RlpList[A] private[rlp] (length: Long) extends RlpItem

private [rlp] object RlpItem {

  def codec = lazily(Codec[RlpItem](encode _, decode _))

  def encode(item: RlpItem): Attempt[BitVector] = item match {
    case str: RlpString[_] => RlpString.codec.encode(str)
    case list: RlpList[_] => RlpList.codec.encode(list)
  }

  def decode(buffer: BitVector): Attempt[DecodeResult[RlpItem]] = for {
    hb <- RlpHeaderByte.codec.decode(buffer)
    decoded <- hb.value match {
      case ValueHeader(_) | StringHeader() => RlpString.codec.decode(buffer)
      case ListHeader() => RlpList.codec.decode(buffer)
    }
  } yield decoded
}

private [rlp] object RlpString {

  def apply(n: Boolean): RlpString[Boolean] = RlpString(BitVector.fromByte(if (n) 1 else 0))
  def apply(n: Byte): RlpString[Byte] = RlpString(BitVector(n))
  def apply(n: Char): RlpString[Char] = RlpString(BitVector(n))
  def apply(n: Short): RlpString[Short] = RlpString(BitVector.fromShort(n, leftTrimmedBytesLength(n) * 8))
  def apply(n: Int): RlpString[Int] = RlpString(BitVector.fromInt(n, leftTrimmedBytesLength(n) * 8))
  def apply(n: Long): RlpString[Long] = RlpString(BitVector.fromLong(n, leftTrimmedBytesLength(n) * 8))
  def apply(n: BigInt): RlpString[BigInt] = RlpString(BitVector(n.toByteArray))

  def codec[A] = Codec[RlpString[A]](encode[A] _, decode[A] _)

  def encode[A](str: RlpString[A]): Attempt[BitVector] = {
    val hb = RlpHeaderByte(str)
    for {
      header <- RlpHeaderByte.codec.encode(hb)
      encoded <- hb match {
        case ValueHeader(v) => Attempt.successful(header)
        case ShortStringHeader(len) => Attempt.successful(header ++ str.bits)
        case LongStringHeader(lenlen) => for {
          len <- ulong(lenlen * 8).encode(str.bits.bytes.length)
        } yield header ++ len ++ str.bits
      }
    } yield encoded
  }

  def decode[A](buffer: BitVector): Attempt[DecodeResult[RlpString[A]]] = for {
    hb <- RlpHeaderByte.codec.decode(buffer)
    decoded <- decodeFromHeader[A](hb)
  } yield decoded

  def decodeFromHeader[A](header: DecodeResult[RlpHeaderByte]): Attempt[DecodeResult[RlpString[A]]] = for {
    decoded <- header.value match {
      case ValueHeader(v) => Attempt.successful(DecodeResult(RlpString[A](BitVector.fromInt(v, 8)), header.remainder))
      case ShortStringHeader(len) => codecs.bits(len * 8).decode(header.remainder).map(_.map(RlpString.apply[A]))
      case LongStringHeader(lenlen) => for {
        len <- ulong(lenlen * 8).decode(header.remainder)
        str <- codecs.bits(len.value.toInt * 8).decode(len.remainder).map(_.map(RlpString.apply[A]))
      } yield str
    }
  } yield decoded

}

private [rlp] object RlpList {

  // Length codecs
  def shortLengthCodec(base: Int) = uint8.xmap[Int](_ - base, _ + base)
  def longLengthCodec(base: Int) = variableSizeBytes(shortLengthCodec(base), LeftTrimmedLongCodec)

  val shortLength = shortLengthCodec(0xc0)
  val longLength = longLengthCodec(0xf7)

  def codec[A] = Codec[RlpList[A]](encode[A] _, decode[A] _)

  def encode[A](list: RlpList[A]): Attempt[BitVector] = {
    val hb = RlpHeaderByte(list)
    for {
      header <- RlpHeaderByte.codec.encode(hb)
      encoded <- hb match {
        case ShortListHeader(len) => shortLength.encode(list.length.toInt)
        case LongListHeader(lenlen) => longLength.encode(list.length)
      }
    } yield encoded
  }

  def decode[A](buffer: BitVector): Attempt[DecodeResult[RlpList[A]]] = for {
    hb <- RlpHeaderByte.codec.decode(buffer)
    decoded <- decodeFromHeader[A](hb)
  } yield decoded

  def decodeFromHeader[A](header: DecodeResult[RlpHeaderByte]): Attempt[DecodeResult[RlpList[A]]] = for {
    decoded <- header.value match {
      case ShortListHeader(len) => shortLength.decode(header.remainder).map(_.map(l => RlpList[A](l.toLong)))
      case LongListHeader(lenlen) => longLength.decode(header.remainder).map(_.map(RlpList.apply[A]))
    }
  } yield decoded

}
