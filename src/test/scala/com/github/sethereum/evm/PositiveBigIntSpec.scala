package com.github.sethereum.evm

import org.scalatest.{Matchers, WordSpec}

class PositiveBigIntSpec extends WordSpec with Matchers {

  "Positive big integer" should {

    "succeed for positive values" in {
      PositiveBigInt(1) shouldBe PositiveBigInt(1)
      (2: BigInt) + PositiveBigInt(1) shouldBe (PositiveBigInt(3): BigInt)
    }

    "fail for non-positive values" in {
      an [IllegalArgumentException] should be thrownBy PositiveBigInt(0)
      an [IllegalArgumentException] should be thrownBy PositiveBigInt(-1)
    }
  }

}
