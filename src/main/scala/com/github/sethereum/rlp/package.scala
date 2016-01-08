package com.github.sethereum

import java.lang.{Integer => javaInt, Long => javaLong}
import java.nio.charset.Charset

import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

import scala.math.Numeric._

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

  def rlist[A](itemCodec: RlpCodec[A]): RlpCodec[List[A]] = RlpCodec(
    rlength(192).consume[List[A]] { _ match {
      case Left(bad) => fail(Err(s"invalid RLP list header $bad"))
      case Right(len) => fixedSizeBytes(len, list(itemCodec))
    }} { b =>
      Right(b.length)
    }
  )

  def rscalar[A : Integral](codec: Codec[A], validateBytes: ValidationFunc[ByteVector], validateValue: ValidationFunc[A]): RlpCodec[A] = {
    RlpCodec(rbytes.exmap[A](
      { bytes => for {
        b <- validateBytes(bytes)
        d <- codec.decode(b.bits)
        v <- validateValue(d.value)
      } yield v },
      i => validateValue(i).flatMap[ByteVector](i => codec.encode(i).map(_.bytes))
    ))
  }

  val rbytearray: RlpCodec[Array[Byte]] = RlpCodec(rbytes.xmap[Array[Byte]](_.toArray, ByteVector.apply))

  def rbytearray(size: Int): RlpCodec[Array[Byte]] = {
    def validate(a: Array[Byte]): Attempt[Array[Byte]] = {
      if (a.length != size) Failure(Err(s"invalid fixed size RLP byte array (expected: $size, actual: ${a.length}"))
      else Successful(a)
    }
    RlpCodec(rbytearray.exmap(validate _, validate _))
  }

  def rstring(implicit charset: Charset): RlpCodec[String] = RlpCodec(rbytes.narrow[String](
    s => string.decode(s.bits).map(_.value),
    s => ByteVector(s.getBytes(charset)))
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


  // Validation functions

  type ValidationFunc[A] = A => Attempt[A]

  implicit def lift[A](f: ValidationFunc[A]): Attempt[A] => Attempt[A] = (attempt: Attempt[A]) => attempt.flatMap(f)
  implicit def liftDecodeResult[A](f: ValidationFunc[A]): Attempt[DecodeResult[A]] => Attempt[DecodeResult[A]] =
    (attempt: Attempt[DecodeResult[A]]) => attempt.flatMap(r => f(r.value).map(_ => r))

  private def pass[A](a: A) = Successful(a)

  private def unlimited[A] = pass[A] _

  private def validateScalarBytes(bits: Int): ByteVector => Attempt[ByteVector] = {
    val max = (bits + 7) / 8
    (bytes: ByteVector) => {
      if (bytes.length > 1 && bytes(0) == 0) Failure(Err(s"scalar byte array contains leading zero(s) $bytes"))
      else if (bytes.length > max) Failure(Err(s"scalar byte array exceeds maximum length (bits: $bits, max: $max, bytes: $bytes)"))
      else Successful(bytes)
    }
  }

  private def validateRange[A](bits: Int, limit: A)(implicit integral: Integral[A]): A => Attempt[A] = {
    import integral._
    (value: A) => {
      if (value < zero || value > limit) Failure(Err(s"scalar value negative or out of range (bits: $bits, value: $value"))
      else Successful(value)
    }
  }

  private def validateNonNegative[A](implicit integral: Integral[A]): A => Attempt[A] = {
    import integral._
    (value: A) => {
      if (value < zero) Failure(Err(s"negative scalar value $value"))
      else Successful(value)
    }
  }

  // Raw BigInt codec
  private val bigint = Codec[BigInt](
    { i: BigInt =>
      val bytes = i.toByteArray
      if (bytes.length > 1 && bytes(0) == 0) Successful(BitVector(bytes.tail))   // Ensure no leading zero
      else Successful(BitVector(bytes))
    },
    { bits: BitVector => bytes.decode(bits).map(_.map(v => BigInt(1, v.toArray))) }
  )

  def ruint32: RlpCodec[Long] = rscalar(uint32, validateScalarBytes(javaInt.SIZE), validateRange(javaInt.SIZE, (1 << javaInt.SIZE) - 1))

  def ruint(bits: Int): RlpCodec[Int] =
    rscalar(uint(bits), validateScalarBytes(bits), validateRange(bits, (1 << bits) - 1))

  def rulong(bits: Int): RlpCodec[Long] =
    rscalar(ulong(bits), validateScalarBytes(bits), validateRange(bits, (1L << bits) - 1))

  val rbigint: RlpCodec[BigInt] = rscalar(bigint, unlimited, validateNonNegative _)

  def rbigint(bits: Int): RlpCodec[BigInt] =
    rscalar(bigint, validateScalarBytes(bits), validateRange(bits, (BigInt(1) << bits) - 1))

}
