package com.github.sethereum.rlp

import com.github.sethereum.rlp
import scodec.codecs._

/**
 * RLP encoding header byte representation.
 *
 * The first byte of an RLP item is used to contain the value or indicate the type of item represented.
 * An item can be one of 5 types:
 * - value
 * - short string
 * - long string
 * - short list
 * - long list
 *
 * @param header first parsed byte
 */
final class RlpHeaderByte private (val header: Int) extends AnyVal {

  def isValue       = (0x00 to 0x7f) contains header
  def isShortString = (0x80 to 0xb7) contains header
  def isLongString  = (0xb8 to 0xbf) contains header
  def isShortList   = (0xc0 to 0xf7) contains header
  def isLongList    = (0xf8 to 0xff) contains header

  def isString = isShortString || isLongString
  def isList   = isShortList   || isLongList
  def isShort  = isShortString || isShortList
  def isLong   = isLongString  || isLongList

  def value                  = if (isValue)       Some(header)        else None
  def shortStringLength      = if (isShortString) Some(header - 0x80) else None
  def longStringLengthLength = if (isLongString)  Some(header - 0xb7) else None
  def shortListLength        = if (isShortList)   Some(header - 0xc0) else None
  def longListLengthLength   = if (isLongList)    Some(header - 0xf7) else None

  def longLengthLength = longStringLengthLength orElse longListLengthLength
  def stringLength     = shortStringLength      orElse longStringLengthLength
  def listLength       = shortListLength        orElse longListLengthLength
}

object RlpHeaderByte {

  val codec = uint8.xmap[RlpHeaderByte](RlpHeaderByte.apply, _.header)

  def apply(header: Int): RlpHeaderByte = {
    require(header >= 0 && header <= 0xff, f"invalid header byte 0x$header%02x")
    new RlpHeaderByte(header)
  }

  def apply(value: RlpString[_]): RlpHeaderByte = {
    val bytes = value.bits.bytes
    val length = bytes.length
    val first = if (length == 1) Some(uint8.decode(value.bits).require.value) else None

    first.filter(_ <= 0x7f).map(RlpHeaderByte.apply)                // Value (single byte string)
      .getOrElse { length match {
        case l if l < 56 => RlpHeaderByte(0x80 + l)                 // Short string
        case l => RlpHeaderByte(0xb7 + leftTrimmedBytesLength(l))   // Long string
      }}
  }

  def apply(list: RlpList[_]): RlpHeaderByte = list.length match {
    case l if l < 56 => RlpHeaderByte(0xc0 + l.toInt)               // Short list
    case l => RlpHeaderByte(0xf7 + rlp.leftTrimmedBytesLength(l))   // Long list
  }

  def apply(item: RlpItem): RlpHeaderByte = item match {
    case s: RlpString[_] => apply(s)
    case l: RlpList[_] => apply(l)
  }

}


// Pattern matching helper objects

object ValueHeader       { def unapply(hb: RlpHeaderByte) = hb.value }
object StringHeader      { def unapply(hb: RlpHeaderByte) = hb.isString }
object ListHeader        { def unapply(hb: RlpHeaderByte) = hb.isList }
object ShortStringHeader { def unapply(hb: RlpHeaderByte) = hb.shortStringLength }
object LongStringHeader  { def unapply(hb: RlpHeaderByte) = hb.longStringLengthLength }
object ShortListHeader   { def unapply(hb: RlpHeaderByte) = hb.shortListLength }
object LongListHeader    { def unapply(hb: RlpHeaderByte) = hb.longListLengthLength }
