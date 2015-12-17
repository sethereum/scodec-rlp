package com.github.sethereum.evm

import com.github.sethereum.EvmSpecBase
import com.github.sethereum.evm.EvmWord.ImplicitConversions._
import org.scalatest.WordSpec

import scala.util.Try

import scodec.bits._

class EvmStateSpec extends WordSpec with EvmSpecBase {

  val ZERO = BigInt(0)
  val ONE = BigInt(1)

  "EVM state stack" should {

    "not allow underflow" in {
      EvmState().pop[EvmWord].failure.exception shouldBe a [StackUnderflowException]
    }

    "not allow overflow on push" in {
      val result = (0 to EvmState.MaxStackSize).foldLeft(Try(EvmState())) { (s, n) =>
        s.flatMap(_.push(BigInt(n)))
      }
      result.failure.exception shouldBe a [StackOverflowException]
    }

    "not allow overflow on creation" in {
      Try(EvmState(stack = List.fill(EvmState.MaxStackSize + 1)(ZERO)))
        .failure.exception shouldBe a [StackOverflowException]
    }

    "push/pop BigInteger" in {
      val value = ONE
      val begin = EvmState()
      val expected = EvmState(stack = List((ONE.toByteArray: EvmWord)))

      val actual = begin.push(ONE).success.value

      actual shouldBe expected

      actual.pop[BigInt].success.value shouldBe (ONE, begin)
    }

  }

  "EVM state storage" should {

    "retrieve ZERO for non-existent key" in {
      EvmState().sget(hex"0011223344".toArray).success.value shouldBe EvmWord.ZERO
    }

    "store/retrieve" in {
      val (key, value) = (EvmStorage.Key(hex"00112233".toArray), EvmStorage.Value(hex"5544332211".toArray))

      val after = EvmState().sput(key, value).success.value
      val actual = after.sget(key).success.value

      actual shouldBe value
    }
  }

}
