package com.github.sethereum

import java.lang.{Integer => javaInt, Long => javaLong}
import java.nio.charset.Charset

import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import shapeless.HList

import scala.language.implicitConversions
import scala.math.Numeric._

/**
 * Codec support for Recursive Length Prefix (RLP) encoding, as defined by the Ethereum project.
 *
 * See: https://github.com/ethereum/wiki/wiki/RLP
 * See: http://gavwood.com/paper.pdf (Appendix B)
 */
package object rlp {

  /**
   * Marker trait used to indicate codecs that encode/decode data using RLP encoding.
   *
   * This trait is used to help enforce type safe RLP codec construction.
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

  // Supporting functions and codecs

  /**
   * Codec for RLP length prefix, which either contains either a value < 128 or a length.
   *
   * @param offset
   * @return Left if length field contains a value, otherwise a Right containing the field length
   */
  private def rlength(offset: Int): Codec[Either[Int, Long]] = uint8.consume[Either[Int, Long]] { h =>
    if (h < offset) provide(Left(h))
    else if ((h - offset) < 56) provide(Right(h.toLong - offset))
    else ulong((h - offset - 55) * 8).xmap(Right.apply, _.right.get)
  } { _ match {
    case Left(l) => l.toInt
    case Right(l) if (l < 56) => l.toInt + offset
    case Right(l) => scalarLength(l) + 55 + offset
  }}

  /**
   * Narrows a Long length to an Int for use in standard APIs (arrays and such that all take an Int size).
   *
   * Note that the maximum length is 31 bits since Ints are signed.
   */
  private def rlength31(offset: Int): Codec[Either[Int, Int]] =
    rlength(offset).narrow[Either[Int, Int]](_ match {
      case Left(v) => Successful(Left(v))
      case Right(len) if (len.isValidInt) => Successful(Right(len.toInt))
      case Right(len) => Failure(Err(s"length out of range (length: $len)"))
    }, _.right.map(_.toLong))

  private val rbyteslength = rlength31(128)

  private val rlistlength = rlength31(192).narrow[Int](_ match {
      case Left(bad) => Failure(Err(s"invalid RLP list header $bad"))
      case Right(len) => Successful(len)
    }, Right.apply)

  private val rbytes: RlpCodec[ByteVector] = RlpCodec(
    rbyteslength.consume { _ match {
      case Left(v) => provide(ByteVector(v))    // Single byte value < 128
      case Right(l) => bytes(l)
    }} { b =>
      if (b.length == 1 && b(0) > 0 && b(0) < 128) Left(b(0))
      else Right(b.length)
    }
  )

  // Scalar length and codec functions to ensure left trimmed scalars

  private def scalarLength(value: Int) = math.max(javaInt.SIZE - javaInt.numberOfLeadingZeros(value), 8) / 8
  private def scalarLength(value: Long) = math.max(javaLong.SIZE - javaLong.numberOfLeadingZeros(value), 8) / 8

  // Codec cache to avoid scalar codec instantiation
  private val intCodecs = (1 to javaInt.BYTES - 1).map(n => uint(n * 8)).toArray
  private val longCodecs = (1 to javaLong.BYTES - 1).map(n => ulong(n * 8)).toArray

  private def intCodec(x: Int) = intCodecs(scalarLength(x) - 1)
  private def longCodec(x: Long) = longCodecs(scalarLength(x) - 1)

  private def intCodec(bytes: ByteVector) = intCodecs(bytes.length - 1)
  private def longCodec(bytes: ByteVector) = longCodecs(bytes.length - 1)

  // Raw BigInt codec
  private val bigint = Codec[BigInt](
    { i: BigInt =>
      val bytes = i.toByteArray
      if (bytes.length > 1 && bytes(0) == 0) Successful(BitVector(bytes.tail))   // Ensure no leading zero
      else Successful(BitVector(bytes))
    },
    { bits: BitVector => bytes.decode(bits).map(_.map(v => BigInt(1, v.toArray))) }
  )

  private def rscalar[A : Integral](enc: A => Codec[A], dec: ByteVector => Codec[A], validateBytes: ValidationFunc[ByteVector], validateValue: ValidationFunc[A]): RlpCodec[A] = {
    RlpCodec(rbytes.exmap[A](
      { bytes => for {
        b <- validateBytes(bytes)
        d <- dec(b).decode(b.bits)
        v <- validateValue(d.value)
      } yield v },
      i => validateValue(i).flatMap(i => enc(i).encode(i).map(_.bytes))
    ))
  }

  // Validation functions

  private type ValidationFunc[A] = A => Attempt[A]

  private def unlimited[A](a: A) = Successful(a)

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

  private def validateBits(bits: Int, max: Int) = {
    require(bits > 0 && bits <= max, "bits must be in range [1, $max] for RLP integer (bits: $bits)")
  }


  // ======================================================
  // RLP Codecs
  // ======================================================

  // ------------------------------------------------------
  // Scalar codecs

  // Shortcut single byte encoding
  val ruint8: RlpCodec[Int] = RlpCodec(uint8.consume[Int] { h =>
    if (h < 0) fail(Err(s"negative scalar value $h"))
    else if (h < 128) provide(h)
    else if (h == 129) uint8
    else fail(Err(s"invalid RLP single byte header $h"))
  } { i =>
    if (i < 128) i
    else 129
  })

  def ruint32: RlpCodec[Long] = rscalar(longCodec, longCodec, validateScalarBytes(javaInt.SIZE), validateRange(javaInt.SIZE, (1 << javaInt.SIZE) - 1))

  def ruint(bits: Int): RlpCodec[Int] = {
    validateBits(bits, javaInt.SIZE - 8)
    rscalar(intCodec, intCodec, validateScalarBytes(bits), validateRange(bits, (1 << bits) - 1))
  }

  def rulong(bits: Int): RlpCodec[Long] = {
    validateBits(bits, javaLong.SIZE - 8)
    rscalar(longCodec, longCodec, validateScalarBytes(bits), validateRange(bits, (1L << bits) - 1))
  }

  val rbigint: RlpCodec[BigInt] = rscalar(_ => bigint, _ => bigint, unlimited, validateNonNegative _)

  def rbigint(bits: Int): RlpCodec[BigInt] =
    rscalar(_ => bigint, _ => bigint, validateScalarBytes(bits), validateRange(bits, (BigInt(1) << bits) - 1))

  // ------------------------------------------------------
  // Array and String codecs

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

  // ------------------------------------------------------
  // List and structure codecs

  def rlist[A](itemCodec: RlpCodec[A]): RlpCodec[List[A]] = RlpCodec(variableSizeBytes(rlistlength, list(itemCodec)))

  // RLP structure codec (wrap existing codec in a list)

  def rstruct[A](codec: Codec[A]): RlpCodec[A] = RlpCodec(variableSizeBytes(rlistlength, codec))

  implicit def rhlist[H <: HList](codec: Codec[H]): RlpCodec[H] = rstruct(codec)

}
