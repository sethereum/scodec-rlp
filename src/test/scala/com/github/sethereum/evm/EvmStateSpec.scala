package com.github.sethereum.evm

import java.math.BigInteger

import com.github.sethereum.EvmSpecBase
import org.scalatest.{Matchers, TryValues, WordSpec}
import EvmWordConversions._

import scala.util.Try

class EvmStateSpec extends WordSpec with EvmSpecBase {

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
      val expected = EvmState(stack = List((ONE.toByteArray: EvmWord)))

      val actual = begin.push(ONE).success.value

      actual shouldBe expected

      actual.pop[BigInteger].success.value shouldBe (ONE, begin)
    }

  }

}
