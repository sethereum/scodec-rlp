package com.github.sethereum.rlp

import java.lang.{Integer => javaInt, Long => javaLong, Short => javaShort}

import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._


// Implements zero length and leading zero checks for scalar values
object RlpScalarBytesCodec extends RlpCodec[ByteVector] {

  override def sizeBound: SizeBound = RlpBytesCodec.sizeBound

  override def encode(value: ByteVector): Attempt[BitVector] = {
    // Leading zeros are disallowed (see Appendix B.165)
    if (value.length > 1 && value(0) == 0)
      Failure(Err(s"invalid attempt to encode scalar value with leading zero(s) (value: $value)"))
    else RlpBytesCodec.encode(value)
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[ByteVector]] = {
    RlpBytesCodec.decode(bits).flatMap { result =>
      if (result.value.length == 0) {
        Failure(Err(s"error decoding zero length scalar"))
      } else if (result.value.length > 1 && result.value(0) == 0) {
        // Leading zeros are disallowed (see Appendix B.165)
        Failure(Err(s"error decoding scalar with leading zeros (value: ${result.value})"))
      } else {
        Successful(result)
      }
    }
  }
}


/**
 * Encodes/decodes BigInt values using RLP encoding.
 *
 * @param bits value range bit limit
 */
class RlpBigIntCodec(val bits: Int) extends RlpCodec[BigInt] {
  require(bits > 0, s"bits must be positive")

  val Limit = BigInt(1) << bits

  override def sizeBound: SizeBound = SizeBound.bounded(8, bits)

  override def encode(value: BigInt): Attempt[BitVector] = {
    value.signum match {
      case -1 => Failure(Err(s"encoding negative scalar values is not supported (value: $value)"))
      case 0 => uint8.encode(0)
      case 1 => {
        if (value >= Limit) {
          Failure(Err(s"value of of range (value: $value, limit: $Limit)"))
        } else if (value < 128) {
          uint8.encode(value.intValue())
        } else {
          // Drop leading zero byte, if necessary (see Appendix B.165)
          val b = value.toByteArray
          val p = if (b(0) == 0) b.drop(1) else b
          RlpScalarBytesCodec.encode(ByteVector(p))
        }
      }
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[BigInt]] =
    RlpScalarBytesCodec.decode(bits).map(_.map(b => BigInt(1, b.toArray)))

}


/**
 * Encodes/decodes integral data types.
 */
private [rlp] abstract class RlpIntegralCodec[A : Integral] extends RlpCodec[A] {
  val bits: Int
  val MaxValue: A
  val codec: Codec[A]
  val integral = implicitly[Integral[A]]
  val SingleByteValue = integral.fromInt(128)

  protected def description = s"$bits-bit variable length RLP unsigned integer"

  override def sizeBound = SizeBound.bounded(1, ((bits + 7) / 8) + 1) * 8

  override def toString: String = description

  override def encode(value: A): Attempt[BitVector] = validate(value) flatMap { value =>
    if (integral.lt(value, SingleByteValue)) {
      // Shortcut common case
      uint8.encode(integral.toInt(value))
    } else {
      codec.encode(value).flatMap(b => RlpScalarBytesCodec.encode(b.bytes))
    }
  }

  override def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
    for {
      bytes <- RlpScalarBytesCodec.decode(bits)
      num <- codec.decode(bytes.value.bits)
      validated <- validate(num)
    } yield validated.mapRemainder(_ => bytes.remainder)
  }

  def validate(value: A): Attempt[A] = {
    if (integral.signum(value) == -1) {
      Failure(Err(s"encoding negative scalar values is not supported (value: $value)"))
    } else if (integral.compare(value, MaxValue) > 0) {
      Failure(Err(s"$value is greater than maximum value $MaxValue for $description"))
    } else {
      Successful(value)
    }
  }

  def validate(result: DecodeResult[A]): Attempt[DecodeResult[A]] = {
    validate(result.value).map(_ => result)
  }

}

/**
 * Encodes/decodes Long values using RLP encoding.
 */
class RlpLongCodec(val bits: Int) extends RlpIntegralCodec[Long] {
  require(bits > 0 && bits <= javaLong.SIZE - 1, "bits must be in range [1, 63] for unsigned Long")

  val MaxValue = (1L << bits) - 1
  val codec = LeftTrimmedLongCodec
}


/**
 * Encodes/decodes Int values using RLP encoding.
 */
class RlpIntCodec(val bits: Int) extends RlpIntegralCodec[Int] {
  require(bits > 0 && bits <= javaInt.SIZE - 1, "bits must be in range [1, 31] for unsigned Int")

  val MaxValue = (1 << bits) - 1
  val codec = LeftTrimmedIntCodec

}


/**
 * Encodes/decodes Short values using RLP encoding.
 */
class RlpShortCodec(val bits: Int) extends RlpIntegralCodec[Short] {
  require(bits > 0 && bits <= javaShort.SIZE - 1, "bits must be in range [1, 15] for unsigned Short")

  val MaxValue = ((1 << bits) - 1).toShort
  val codec = LeftTrimmedShortCodec

}
