package com.github.sethereum.evm

import org.scalatest.{Matchers, WordSpec}

class ByteSequenceSpec extends WordSpec with Matchers {

  "B256" should {

    "succeed on valid byte array" in {
      val bytes: Seq[Byte] = B256(Seq.fill(256)(1.toByte))
      bytes.length shouldBe 256
      bytes.forall(_ == 1) shouldBe true
    }

    "fail on invalid byte array" in {
      an [IllegalArgumentException] should be thrownBy B256(Array.fill(255)(1.toByte))
      an [IllegalArgumentException] should be thrownBy B256(Array.fill(257)(1.toByte))
    }
  }

}
