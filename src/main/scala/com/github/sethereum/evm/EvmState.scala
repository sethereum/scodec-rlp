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
    memory: EvmMemory = EvmMemory(),
    storage: EvmStorage = EvmStorage.empty
)
    extends EvmExecutionOps[EvmState]
    with EvmStackOps[EvmState]
    with EvmMemoryOps[EvmState]
    with EvmStorageOps[EvmState]
{

  // Check state invariants
  EvmState.validate(this)

  // Execution operations
  override def jump(pc: Int): Try[EvmState] = execution.jump(pc).map(ex => copy(execution = ex))

  // Stack operations
  override def push[A](a: A)(implicit toW: A => EvmWord) =
    Try(copy(stack = toW(a) +: stack))
  override def pop[A](implicit fromW: EvmWord => A): Try[(A, EvmState)] =
    Try(stack.headOption.map(w => (fromW(w), copy(stack = stack.tail)))
      .getOrElse(throw new StackUnderflowException))

  // Memory operations
  override def memSize: EvmWord = memory.memSize
  override def memLoad(offset: Int): Try[(EvmWord, EvmState)] =
    memory.memLoad(offset).map { case (w, m) => (w, copy(memory = m))}
  override def memSlice(begin: Int, end: Int): Try[(Seq[Byte], EvmState)] =
    memory.memSlice(begin, end).map { case (d, m) => (d, copy(memory = m))}
  override def memStore(offset: Int, value: EvmWord): Try[EvmState] =
    memory.memStore(offset, value).map(m => copy(memory = m))
  override def memStore(offset: Int, value: Byte): Try[EvmState] =
    memory.memStore(offset, value).map(m => copy(memory = m))
  override def memStore(offset: Int, value: Seq[Byte]): Try[EvmState] =
    memory.memStore(offset, value).map(m => copy(memory = m))

  // Storage operations
  override def sget(k: EvmStorage.Key) = Try(storage(k))
  override def sput(k: EvmStorage.Key, v: EvmStorage.Value): Try[EvmState] = Try(copy(storage = storage + (k -> v)))
}


object EvmState {

  val MaxStackSize = 1024

  val environmentLens   = lens[EvmState] >> 'environment
  val callLens          = lens[EvmState] >> 'environment >> 'call
  val executionLens     = lens[EvmState] >> 'execution
  val stackLens         = lens[EvmState] >> 'stack
  val storageLens       = lens[EvmState] >> 'storage

  // Validate state invariants, throwing the appropriate EvmException variant upon violation
  def validate(state: EvmState) = {
    if (state.stack.length > MaxStackSize) throw new StackOverflowException(s"stack exceeded maximum size of $MaxStackSize")
  }
}


sealed trait EvmAccount {
  val nonce: EvmNonce
  val balance: EvmBalance
  val storageRoot: EvmHash
  val codeHash: EvmHash

  // Account validity function (4.1.12)
//  requireNonce(nonce)
//  requireBalance(balance)
//  requireHash(storageRoot)
//  requireHash(codeHash)
}
object EvmAccount {
  def apply(nonce: EvmNonce, balance: EvmBalance, storageRoot: EvmHash, codeHash: EvmHash): EvmAccount = codeHash match {
    case EvmHash.Empty => EvmSimpleAccount(nonce, balance, storageRoot)
    case _ => EvmContractAccount(nonce, balance, storageRoot, codeHash)
  }
}

case class EvmSimpleAccount(nonce: EvmNonce, balance: EvmBalance, storageRoot: EvmHash) extends EvmAccount {
  override val codeHash = EvmHash.Empty
}
case class EvmContractAccount(nonce: EvmNonce, balance: EvmBalance, storageRoot: EvmHash, codeHash: EvmHash) extends EvmAccount


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

