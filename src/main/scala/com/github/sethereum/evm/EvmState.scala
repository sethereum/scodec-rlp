package com.github.sethereum.evm

import com.github.sethereum.evm.EvmStack.StackUnderflowException
import shapeless._

import scala.language.{higherKinds, implicitConversions}
import scala.util.Try


case class EvmState(stack: EvmStack, storage: EvmStorage = EvmStorage.empty)

object EvmState {
  val stackLens   = lens[EvmState] >> 'stack
  val storageLens = lens[EvmState] >> 'storage
}

trait EvmStackOps[T <: EvmStackOps[T]] { this: T =>
  type Word = EvmWord
  def push[A](a: A)(implicit toW: A => Word): Try[T]
  def pop[A](implicit fromW: Word => A): Try[(A, T)]
}

case class EvmStack2(items: List[EvmWord] = List.empty) extends EvmStackOps[EvmStack2] {
  override def push[A](a: A)(implicit toW: (A) => Word): Try[EvmStack2] =
    Try(copy(items = toW(a) +: items))
  override def pop[A](implicit fromW: (Word) => A): Try[(A, EvmStack2)] =
    Try(items.headOption.map(w => (fromW(w), copy(items = items.tail)))
      .getOrElse(throw new StackUnderflowException))
}

object EvmStack2 {
  val MaxSize = 1024
  val empty = EvmStack2()
}

case class EvmState2(stack: EvmStack2 = EvmStack2.empty, storage: EvmStorage = EvmStorage.empty) extends EvmStackOps[EvmState2] {
  import EvmState2._

  override def push[A](a: A)(implicit toW: A => Word): Try[EvmState2] =
    stack.push(a).map(stackLens.set(this))

  override def pop[A](implicit fromW: Word => A): Try[(A, EvmState2)] =
    stack.pop[A].map { case (a, s) => (a, stackLens.set(this)(s)) }
}

object EvmState2 {
  val empty = EvmState2()
  val stackLens   = lens[EvmState2] >> 'stack
  val storageLens = lens[EvmState2] >> 'storage
}

sealed trait EvmPop[L, +F <: L => (EvmState2 => Try[EvmState2])] {
  def apply[A]: EvmPop[L => A, (L => A) => (EvmState2 => Try[EvmState2])]
//  def apply[F](f: F): (EvmState2 => L) => (EvmState2 => Try[EvmState2])
}

object EvmPopUnit extends EvmPop[Unit, Unit => (EvmState2 => Try[EvmState2])] {
  override def apply[A]: EvmPop[(Unit) => A, (Unit => A) => (EvmState2 => Try[EvmState2])] =
    new EvmPopArg[Unit, A, (Unit => A) => (EvmState2 => Try[EvmState2])](this)

//  override def apply[F](f: F): ((EvmState2) => Unit) => (EvmState2) => Try[EvmState2] = ???
}

case class EvmPopArg[L, R, F <: (L => R) => (EvmState2 => Try[EvmState2])](parent: EvmPop[L, L => (EvmState2 => Try[EvmState2])]) extends EvmPop[L => R, F] {
  def apply[A] = new EvmPopArg[L => R, A, ((L => R) => A) => (EvmState2 => Try[EvmState2])](this)
}

//sealed trait EvmPop[L, +F <: (L => EvmState2) => Try[EvmState2]] {
//  def apply[A]: EvmPop[L => A, ((L => A) => EvmState2) => Try[EvmState2]]
//}
//
//object EvmPopUnit extends EvmPop[Unit, (Unit => EvmState2) => Try[EvmState2]] {
//  override def apply[A]: EvmPop[(Unit) => A, ((Unit) => A) => (EvmState2) => Try[EvmState2]] =
//    new EvmPopArg[Unit, A, ((Unit) => A) => (EvmState2) => Try[EvmState2]](this)
//}
//
//case class EvmPopArg[L, R, F <: ((L => R) => EvmState2) => Try[EvmState2]](parent: EvmPop[L, (L => EvmState2) => Try[EvmState2]]) extends EvmPop[L => R, F] {
//  def apply[A] = new EvmPopArg[L => R, A, (((L => R) => A) => EvmState2) => Try[EvmState2]](this)
//}
//
