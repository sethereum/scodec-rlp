package com.github.sethereum.evm

import shapeless.{HNil, _}
import shapeless.ops.hlist._

import scala.language.{higherKinds, implicitConversions}



case class EvmState(stack: EvmStack, storage: EvmStorage = EvmStorage.empty)

object EvmState {
  val stackLens   = lens[EvmState] >> 'stack
  val storageLens = lens[EvmState] >> 'storage
}
