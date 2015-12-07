package com.github.sethereum.evm

import shapeless._

import scala.language.{higherKinds, implicitConversions}
import scala.util.Try



/**
 * Immutable virtual machine state representation.
 *
 * Every operation on the virtual machine (represented as an EvmStateTransitionFunction) produces a new virtual
 * machine state.
 *
 * State invariants are enforced at construction to ensure only valid states can be created.
 *
 * @param stack
 * @param storage
 */
case class EvmState(
    stack: List[EvmWord] = List.empty,
    storage: EvmStorage = EvmStorage.empty
) extends EvmStackOps[EvmState] {

  // Check state invariants
  EvmState.validate(this)

  // Stack operations

  override def push[A](a: A)(implicit toW: A => Word) =
    Try(copy(stack = toW(a) +: stack))

  override def pop[A](implicit fromW: Word => A): Try[(A, EvmState)] =
    Try(stack.headOption.map(w => (fromW(w), copy(stack = stack.tail)))
      .getOrElse(throw new StackUnderflowException))

  // Storage operations
}


object EvmState {

  val MaxStackSize = 1024

  val stackLens   = lens[EvmState] >> 'stack
  val storageLens = lens[EvmState] >> 'storage

  // Validate state invariants, throwing the appropriate EvmException variant upon violation
  def validate(state: EvmState) = {
    if (state.stack.length > MaxStackSize) throw new StackOverflowException(s"stack exceeded maximum size of $MaxStackSize")
  }
}


/**
 * Immutable stack operations for the EVM.
 *
 * Type safe push/pop operations are specified using implicit conversion between machine words and typed operands.
 * Push/pop operations MUST emit Failure on underflow and overflow conditions.
 */
trait EvmStackOps[T <: EvmStackOps[T]] { this: T =>
  type Word = EvmWord
  def push[A](a: A)(implicit toW: A => Word): Try[T]
  def pop[A](implicit fromW: Word => A): Try[(A, T)]
}


class EvmStateException(msg: String) extends EvmException(msg) {
  def this() = this("")
}

class StackOverflowException(msg: String) extends EvmStateException(msg)
class StackUnderflowException extends EvmStateException

