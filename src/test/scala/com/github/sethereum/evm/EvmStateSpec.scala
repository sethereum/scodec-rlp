package com.github.sethereum.evm

import java.math.BigInteger

import org.scalatest.{Matchers, TryValues, WordSpec}
import EvmWordConversions._

import scala.util.Try

class EvmStateSpec extends WordSpec with Matchers with TryValues {

  "EVM state stack" should {

    "not allow underflow" in {
      EvmState().pop[EvmWord].failure.exception shouldBe a [StackUnderflowException]
    }

    "not allow overflow on push" in {
      val result = (0 to EvmState.MaxStackSize).foldLeft(Try(EvmState())) { (s, n) =>
        s.flatMap(_.push(BigInteger.valueOf(n)))
      }
      result.failure.exception shouldBe a [StackOverflowException]
    }

    "not allow overflow on creation" in {
      Try(EvmState(stack = List.fill(EvmState.MaxStackSize + 1)(BigInteger.ZERO)))
        .failure.exception shouldBe a [StackOverflowException]
    }

    "push/pop BigInteger" in {
      import BigInteger._

      val value = ONE
      val begin = EvmState()
      val expected = EvmState(stack = List(ONE.toByteArray))

      val actual = begin.push(ONE).success.value


      actual.stack.length shouldBe expected.stack.length
      actual.stack.zip(expected.stack).foreach { case (a, e) => a should === (e) }

      actual.pop[BigInteger].success.value shouldBe (ONE, begin)
    }

  }

}
