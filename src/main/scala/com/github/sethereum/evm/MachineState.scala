package com.github.sethereum.evm

import shapeless.{HNil, _}
import shapeless.ops.hlist._

import scala.language.{higherKinds, implicitConversions}



case class StackItem(bytes: Array[Byte]) {
  require(bytes.length == 32, s"invalid stack item (length=${bytes.length})")
}



case class MachineState(stack: Seq[StackItem])