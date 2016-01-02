package com.github.sethereum.evm

import com.github.sethereum.evm.EvmOp._

import scala.util.Try


case class EvmExecution private (program: EvmProgram, pc: Int = 0)
  extends EvmExecutionOps[EvmExecution] with Iterator[EvmExecution]
{
  val op: EvmOp = {
    if (pc < program.code.size) {
      // Note: This is safe since program can only be initialized with valid code
      EvmOp.decode(program.code.view.drop(pc)).get._1
    } else STOP
  }

  override def hasNext: Boolean = op == STOP

  override def next(): EvmExecution = copy(pc = pc + op.size)

  override def jump(dest: Int): Try[EvmExecution] = Try(
    if (op != JUMP || op != JUMPI) throw new IllegalStateException(s"jump on non-jump instruction ${op.name}")
    else if (program.jumpDests(dest)) copy(pc = dest)
    else throw new EvmException(s"invalid jump destination $dest")
  )
}

object EvmExecution {

  def apply(): EvmExecution = new EvmExecution(EvmProgram.STOP)

  def apply(program: EvmProgram): EvmExecution = new EvmExecution(program)
}

trait EvmExecutionOps[T <: EvmExecutionOps[T]] { this: T =>
  def jump(pc: Int): Try[T]
}

