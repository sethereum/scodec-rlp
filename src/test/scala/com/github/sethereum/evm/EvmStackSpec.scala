package com.github.sethereum.evm

import java.math.BigInteger

import com.github.sethereum.evm.EvmStack.Conversions._
import org.scalatest.{Matchers, WordSpec}
import shapeless._

class EvmStackSpec extends WordSpec with Matchers {

  "EVM stack" should {

    "push/pop HNil" in {

      val stack = EvmStack()
      stack.push(HNil) shouldBe stack

      stack.pop[HNil] shouldBe Some((HNil), stack)
    }

    "push/pop HList" in {
      val expected = EvmStack(List((BigInteger.ZERO: EvmWord)))

      type Items = BigInteger :: HNil
      val hlist: Items = BigInteger.ZERO :: HNil
      val actual = EvmStack().push(hlist)

      actual.items.length shouldBe expected.items.length
      actual.items.zip(expected.items).foreach { case (a, e) => a should === (e) }

      actual.pop[Items] shouldBe Some((hlist, EvmStack.empty))
    }

  }

}
