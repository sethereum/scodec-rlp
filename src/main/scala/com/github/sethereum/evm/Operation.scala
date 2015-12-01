package com.github.sethereum.evm

import java.math.BigInteger

import shapeless.{HList, Nat}
import shapeless.nat._

import scala.util.{Try}

import StackItemOps._


// Pop function takes stack and produces N item product
// OpCode function takes N item product and maps to M item product
// Push function takes M item product and pushes onto stack
// Combine to form function (Stack) => (Stack)


sealed trait StackFn[Pop <: Nat, Push <: Nat]

object StackFn {

  HList

}
sealed abstract class StackFn_0_0(fn: () => Unit) extends StackFn[_0, _0]


sealed trait OpCodeFunction[Pop <: Nat, Push <: Nat, Code <: OpCode[Pop, Push]] {
  val opcode: Code
}

object Operation {


  implicit class OpCodeOperations(opCode: OpCode) {
    import OpCode._

    def apply(state: MachineState): Try[MachineState] = opCode match {

      case ADD => Try(state.stack match {
        case Seq(l, r, rest @ _ *) => MachineState( (l + r) +: rest)
        case _ => throw new IllegalStateException("stack underflow")
      })

    }
  }

}
