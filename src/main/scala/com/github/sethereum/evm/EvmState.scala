package com.github.sethereum.evm

import shapeless._

import scala.util.Try



/**
 * Immutable virtual machine state representation.
 *
 * Every operation on the virtual machine (represented as an EvmStateTransitionFunction) produces a new virtual
 * machine state.
 *
 * State invariants are enforced at construction to ensure only valid states can be created.
 *
 */
case class EvmState(
    environment: EvmEnvironment = EvmEnvironment(),
    execution: EvmExecution = EvmExecution(),
    stack: List[EvmWord] = List.empty,
    storage: EvmStorage = EvmStorage.empty
) extends EvmExecutionOps[EvmState] with EvmStackOps[EvmState] with EvmStorageOps[EvmState] {

  // Check state invariants
  EvmState.validate(this)

  // Execution operations
  override def stop: Try[EvmState] = execution.stop.map(ex => copy(execution = ex))

  // Stack operations
  override def push[A](a: A)(implicit toW: A => EvmWord) =
    Try(copy(stack = toW(a) +: stack))

  override def pop[A](implicit fromW: EvmWord => A): Try[(A, EvmState)] =
    Try(stack.headOption.map(w => (fromW(w), copy(stack = stack.tail)))
      .getOrElse(throw new StackUnderflowException))

  // Storage operations
  override def sget(k: EvmStorage.Key) = Try(storage(k))
  override def sput(k: EvmStorage.Key, v: EvmStorage.Value): Try[EvmState] = Try(copy(storage = storage + (k -> v)))
}


object EvmState {

  val MaxStackSize = 1024

  val environmentLens   = lens[EvmState] >> 'environment
  val executionLens     = lens[EvmState] >> 'execution
  val stackLens         = lens[EvmState] >> 'stack
  val storageLens       = lens[EvmState] >> 'storage

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
  def push[A](a: A)(implicit toW: A => EvmWord): Try[T]
  def pop[A](implicit fromW: EvmWord => A): Try[(A, T)]
}

trait EvmStorageOps[T <: EvmStorageOps[T]] { this: T =>
  import EvmStorage._
  def sget(k: Key): Try[Value]
  def sput(k: Key, v: Value): Try[T]
}


class EvmStateException(msg: String) extends EvmException(msg) {
  def this() = this("")
}

class StackOverflowException(msg: String) extends EvmStateException(msg)
class StackUnderflowException extends EvmStateException

