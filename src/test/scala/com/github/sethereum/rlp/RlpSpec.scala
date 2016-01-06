package com.github.sethereum.rlp

import java.nio.ByteBuffer
import java.nio.charset.Charset

import com.github.sethereum.rlp
import org.scalatest.{Matchers, WordSpec}
import scodec.bits._
import scodec.{Attempt, DecodeResult}
import shapeless.HNil

class RlpSpec extends WordSpec with Matchers {

  // Drop leading zeroes
  def trimmedBytes(value: Long): Array[Byte] = {
    val buf = ByteBuffer.allocate(8).putLong(value).array()
    buf.slice(Math.min(buf.indexWhere(_ != 0), buf.size - 1), buf.size)
  }

  "Rlp decoder" should {

    "decode single byte array" in {
      for (b <- (0x00 to 0x7f)) {
        val bits = BitVector(b)
        val expected = bits.bytes.toArray

        rlpBytes.decode(bits).map(_.map(_.toSeq)) shouldBe Attempt.successful(DecodeResult(expected.toSeq, BitVector.empty))
      }
    }

    "decode empty byte array" in {
      val bits = BitVector(0x80)
      val expected = Array[Byte]()

      rlpBytes.decode(bits).map(_.map(_.toSeq)) shouldBe Attempt.successful(DecodeResult(expected.toSeq, BitVector.empty))
    }

    "decode single byte short array" in {
      for (b <- (0x80 to 0xff)) {
        val bits = BitVector(0x80 + 1, b)
        val expected = bits.bytes.drop(1).toArray

        rlpBytes.decode(bits).map(_.map(_.toSeq)) shouldBe Attempt.successful(DecodeResult(expected.toSeq, BitVector.empty))
      }
    }

    "decode 2 or more byte short strings" in {
      val lengthRange = 2 to 55
      val byteRange = 0 to 0xff

      for (len <- lengthRange; b <- byteRange) {
        val header = 0x80 + len
        val bits = BitVector(Stream(header.toByte) ++ Stream.fill(len)(b.toByte))
        val expected = bits.bytes.drop(1).toArray

        rlpBytes.decode(bits).map(_.map(_.toSeq)) shouldBe Attempt.successful(DecodeResult(expected.toSeq, BitVector.empty))
      }
    }

    "decode long strings" in {
      val lengthRange = 0xb7 + 1 to 0xb7 + 100000 by 100
      val byteRange = 0 to 0xff by 128

      for (len <- lengthRange; b <- byteRange) {
        val lenBytes = trimmedBytes(len)
        val header = (0xb8 - 1 + lenBytes.size).toByte
        val bits = BitVector(Iterator.single(header) ++ lenBytes.iterator ++ Iterator.fill(len)(b.toByte))
        val expected = bits.bytes.drop(1 + lenBytes.size).toArray

        rlpBytes.decode(bits).map(_.map(_.toSeq)) shouldBe Attempt.successful(DecodeResult(expected.toSeq, BitVector.empty))
      }
    }

  }

  "RLP list codec" should {

    "roundtrip an empty list" in {
      val bits = BitVector(0xc0)
      val expected = List[Array[Byte]]()

      rlpList(rlpBytes).decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      rlpList(rlpBytes).decode(bits).flatMap(r => rlpList(rlpBytes).encode(r.value)) shouldBe Attempt.successful(bits)
    }

    "roundtrip a short List" in {
      val bits = BitVector(0xc3, 0x00, 0x01, 0x02)
      val expected = List(0, 1, 2)
      val codec = rlpList(rlpInt(8))

      codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      codec.decode(bits).flatMap(r => codec.encode(r.value)) shouldBe Attempt.successful(bits)
    }

    "roundtrip a short HList" in {
      val bits = BitVector(0xc4, 0x00, 0x01, 0x81, 0x85)
      val expected = 0 :: 1 :: 0x85 :: HNil
      val codec = rlpHList(rlpInt(8) :: rlpInt(8) :: rlpInt(8))

      codec.decode(bits) shouldBe Attempt.successful(DecodeResult(expected, BitVector.empty))
      codec.decode(bits).flatMap(r => codec.encode(r.value)) shouldBe Attempt.successful(bits)
    }

  }

}

