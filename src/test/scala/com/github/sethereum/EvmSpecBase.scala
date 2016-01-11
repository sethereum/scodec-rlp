package com.github.sethereum

import com.github.sethereum.evm.{EvmState, EvmWord}
import org.scalatest.{TryValues, Matchers}
import EvmWord.ImplicitConversions._

trait EvmSpecBase extends Matchers with TryValues{

  def makeState(xs: BigInt*): EvmState =
    xs.foldLeft(EvmState()) { (s, x) => s.push(x).success.value }

}
