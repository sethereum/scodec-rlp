package com.github.sethereum.evm

import java.math.BigInteger

import scala.util.{Try}

import StackItemOps._

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
