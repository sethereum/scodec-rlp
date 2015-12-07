package com.github.sethereum.evm

import java.math.BigInteger._

import com.github.sethereum.EvmSpecBase
import org.scalatest.WordSpec
import org.scalatest.exceptions.TestFailedException

class EvmOpSpec extends WordSpec with EvmSpecBase {

  def checkOperation(op: EvmOp, before: EvmState, after: EvmState) =
    op(before).success.value shouldBe after


  "EVM operation" should {

    import EvmOp._

    "ADD" in {
      checkOperation(ADD, makeState(ONE, ONE), makeState(valueOf(2)))
      a [TestFailedException] should be thrownBy checkOperation(ADD, makeState(ONE, ZERO), makeState(valueOf(2)))
    }


  }

}
