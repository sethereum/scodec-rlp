package com.github.sethereum

import java.lang.{Integer => javaInt, Long => javaLong}
import java.nio.charset.Charset

import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

import scala.language.implicitConversions

/**
 * Codec support for RLP encoding, as defined by the Ethereum project.
 *
 * See: https://github.com/ethereum/wiki/wiki/RLP
 * See: http://gavwood.com/paper.pdf (Appendix B)
 */
package object rlp {


  // Basic data type codecs

  implicit val rlpBytes = RlpCodec(RlpBytesCodec.xmap[Array[Byte]](_.toArray, ByteVector.apply))
  def rlpBytes(bytes: Int) = new RlpFixedSizeBytesCodec(bytes).xmap[Array[Byte]](_.toArray, ByteVector.apply)

  def rlpShort(bits: Int) = new RlpShortCodec(bits)
  def rlpInt(bits: Int) = new RlpIntCodec(bits)
  def rlpLong(bits: Int) = new RlpLongCodec(bits)
  def rlpBigInt(bits: Int) = new RlpBigIntCodec(bits)

  def rlpString(implicit charset: Charset): RlpCodec[String] = RlpCodec(RlpBytesCodec.narrow[String](
    s => string.decode(s.bits).map(_.value),
    s => ByteVector(s.getBytes(charset)))
  )

  // List codec

  implicit def rlpList[A](implicit itemsCodec: RlpCodec[A]): RlpCodec[List[A]] = new RlpListCodec[A](itemsCodec)

  // RLP structure codec (wrap existing codec in a list)

  def rlpStruct[A](codec: Codec[A]): RlpCodec[A] = new RlpStructCodec[A](codec)

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

  def rlength(offset: Int): Codec[Either[Int, Long]] = uint8.consume[Either[Int, Long]] { h =>
    if (h < offset) provide(Left(h))
    else if ((h - offset) < 56) provide(Right(h.toLong - offset))
    else ulong((h - offset - 55) * 8).xmap[Either[Int, Long]](Right.apply, _.right.get)
  } { _ match {
    case Left(l) => l.toInt
    case Right(l) if (l < 56) => l.toInt + offset
    case Right(l) => leftTrimmedBytesLength(l) + 55 + offset
  }}

  val rbytes: RlpCodec[ByteVector] = RlpCodec(
    rlength(128).consume { _ match {
      case Left(v) => provide(ByteVector(v))    // Single byte value < 128
      case Right(l) => bytes(l.toInt)
    }} { b =>
      if (b.length == 1 && b(0) > 0 && b(0) < 128) Left(b(0))
      else Right(b.length)
    }
  )

  val rbytearray: RlpCodec[Array[Byte]] = RlpCodec(rbytes.xmap[Array[Byte]](_.toArray, ByteVector.apply))

  def rbytearray(size: Int): RlpCodec[Array[Byte]] = {
    def validate(a: Array[Byte]): Attempt[Array[Byte]] = {
      if (a.length != size) Failure(Err(s"invalid fixed size RLP byte array (expected: $size, actual: ${a.length}"))
      else Successful(a)
    }
    RlpCodec(rbytearray.exmap(validate _, validate _))
  }

  val rscalarbytes: RlpCodec[ByteVector] = RlpCodec(rbytes.narrow(validateNoLeadingZeros, identity))

  def rscalar[A : Integral](bits: Int, codec: Codec[A]): RlpCodec[A] = rscalar(Some(bits), codec)

  def rscalar[A : Integral](tooLong: LengthValidator[A], codec: Codec[A]): RlpCodec[A] = {
    RlpCodec(rscalarbytes.exmap[A](_ match {
      case bytes @ tooLong(len, max) => Failure(Err(s"scalar byte array exceeds maximum length (len: $len, max: $max)"))
      case bytes => codec.decode(bytes.bits) match {
        case num @ negative() => Failure(Err(s"negative scalar value $num"))
        //case num @ tooLong(len, max) => Failure(Err(s"value exceeds maximum bit length (bits: $bits, value: $i)"))
      }},
//      { bytes =>
//        bits.flatMap { bits =>
//          val maxBytes = ((bits + 7) / 8)
//          if (bytes.length > maxBytes) {
//            Some(Failure(Err(s"scalar byte array exceeds maximum length (max: $maxBytes, size: ${bytes.length})")))
//          } else None
//        } getOrElse {
//          codec.decode(bytes.bits).flatMap(r => validatePositive(r.value))
//        }
//      },
      i => validatePositive(i).flatMap[ByteVector](i => codec.encode(i).map(_.bytes))
    ))
  }

  def rlist[A](itemCodec: RlpCodec[A]): RlpCodec[List[A]] = RlpCodec(
    rlength(192).consume[List[A]] { _ match {
      case Left(bad) => fail(Err(s"invalid RLP list header $bad"))
      case Right(len) => fixedSizeBytes(len, list(itemCodec))
    }} { b =>
      Right(b.length)
    }
  )

  val ruint8: RlpCodec[Int] = RlpCodec(uint8.consume[Int] { h =>
    if (h < 0) fail(Err(s"negative scalar value $h"))
    else if (h < 128) provide(h)
    else if (h == 129) uint8
    else fail(Err(s"invalid RLP single byte header $h"))
  } { i =>
    if (i < 128) i
    else 129
  })

  def validateNoLeadingZeros(a: ByteVector): Attempt[ByteVector] =
    if (a.length > 1 && a(0) == 0) Failure(Err(s"invalid leading zero(s) $a"))
    else Successful(a)

  def validatePositive[A : Integral](i: A): Attempt[A] = {
    val integral = implicitly[Integral[A]]
    if (integral.lt(i, integral.zero)) Failure(Err(s"negative scalar value $i"))
    else Successful(i)
  }

  object negative {
    def unapply[A](r: Attempt[DecodeResult[A]])(implicit integral: Integral[A]) =
      r.toOption.map(r => integral.lt(r.value, integral.zero))
  }

  def test[A: Integral](validate: LengthValidator[A], i: A): Boolean = {
    val bytes = ByteVector(0)
    bytes match {
      case validate() => false
      case _ => true
    }
  }

  type ValidationFunc[A] = A => Either[Failure, A]

  sealed abstract class LengthValidator[A : Integral](b: ValidationFunc[ByteVector], v: ValidationFunc[A]) {
    def apply(bytes: ByteVector) = b(bytes)
    def apply(value: A) = v(value)
  }

  def maxFailure[A](length: A => Int, max: Int, f: => Failure)(value: A) =
    if (length(value) > max) Left(f) else Right(value)

  def sizeGreaterThanBytes(bits: Int)(bytes: ByteVector) = maxOption(bytes.length, ((bits + 7) / 8))

  Failure(Err(s"scalar byte array exceeds maximum length (len: $len, max: $max)"))

  def unlimitedLength[A : Integral] = new LengthValidator[A](_ => None, _ => None) {}

  def invalidBigInt(bits: Int) = new LengthValidator[BigInt](sizeGreaterThanBytes(bits), i => maxOption(i.bitLength, bits - 1)) {}

  def invalidInt(bits: Int) = new LengthValidator[Int](sizeGreaterThanBytes(bits), i => maxOption(javaInt.highestOneBit(i), bits - 1)) {}

  def invalidLong(bits: Int) = new LengthValidator[Long](sizeGreaterThanBytes(bits), i => maxOption(javaLong.highestOneBit(i), bits - 1)) {}


  def validateBigIntLength(bits: Int)(i: BigInt): Attempt[BigInt] =
    if (i.bitLength < bits) Successful(i)
    else Failure(Err(s"value exceeds maximum bit length (bits: $bits, value: $i)"))

  def validateIntLength(bits: Int)(i: Int): Attempt[Int] =
    if (javaInt.highestOneBit(i) < bits) Successful(i)
    else Failure(Err(s"value exceeds maximum bit length (bits: $bits, value: $i)"))

  def validateLongLength(bits: Long)(i: Long): Attempt[Long] =
    if (javaLong.highestOneBit(i) < bits) Successful(i)
    else Failure(Err(s"value exceeds maximum bit length (bits: $bits, value: $i)"))

  def ruint32: RlpCodec[Long] = rscalar(32, uint32)

  def ruint(bits: Int): RlpCodec[Int] =
    rscalar(bits, uint(bits).exmap[Int](validateIntLength(bits), validateIntLength(bits)))

  def rulong(bits: Int): RlpCodec[Long] =
    rscalar(bits, ulong(bits).exmap[Long](validateLongLength(bits), validateLongLength(bits)))

  private val bigint = Codec[BigInt](
    { i: BigInt =>
      val bytes = i.toByteArray
      if (bytes.length > 1 && bytes(0) == 0) Successful(BitVector(bytes.tail))   // Ensure no leading zero
      else Successful(BitVector(bytes))
    },
    { bits: BitVector => bytes.decode(bits).map(_.map(v => BigInt(1, v.toArray))) }
  )

  private

  val rbigint: RlpCodec[BigInt] = rscalar(None, bigint)

  def rbigint(bits: Int): RlpCodec[BigInt] =
    rscalar(bits, bigint.exmap[BigInt](validateBigIntLength(bits), validateBigIntLength(bits)))

}
