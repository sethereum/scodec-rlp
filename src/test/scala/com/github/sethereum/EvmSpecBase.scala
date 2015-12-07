package com.github.sethereum

import java.math.BigInteger

import com.github.sethereum.evm.{EvmWordConversions, EvmState}
import org.scalatest.{TryValues, Matchers}
import EvmWordConversions._

trait EvmSpecBase extends Matchers with TryValues{

  def makeState(xs: BigInteger*): EvmState =
    xs.foldLeft(EvmState()) { (s, x) => s.push(x).success.value }

}
