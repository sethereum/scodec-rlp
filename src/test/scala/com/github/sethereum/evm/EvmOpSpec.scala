package com.github.sethereum.evm

import java.math.BigInteger
import java.math.BigInteger._

import com.github.sethereum.evm.EvmWordConversions._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, TryValues, WordSpec}

class EvmOpSpec  extends WordSpec with Matchers with TryValues {

  def makeState(xs: BigInteger*): EvmState =
    xs.foldLeft(EvmState()) { (s, x) => s.push(x).success.value }

  def checkOperation(op: EvmOp, before: EvmState, after: EvmState) = {
    val actual = op(before).success.value

    actual.stack.length shouldBe after.stack.length
    actual.stack.zip(after.stack).foreach { case (a, e) => a should === (e) }
  }


  "EVM operation" should {

    import EvmOp._

    "ADD" in {

      checkOperation(ADD, makeState(ONE, ONE), makeState(valueOf(2)))

      a [TestFailedException] should be thrownBy checkOperation(ADD, makeState(ONE, ZERO), makeState(valueOf(2)))
    }


  }

}
