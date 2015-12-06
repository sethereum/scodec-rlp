package com.github.sethereum.evm

import java.math.BigInteger

import shapeless.ops.hlist.{IsHCons, RightFolder}
import shapeless.ops.tuple.{Prepend, IsComposite, RightFolder => RightFolderT}
import shapeless._
import shapeless.syntax.std.tuple._

import scala.language.implicitConversions
import scala.util.Try

/**
 * Immutable stack implementation for the EVM.
 *
 * Type safe push/pop operations are provided via an HList interface.
 * Push/pop operations throw exception on underflow and overflow conditions.
 *
 * @param items
 */
case class EvmStack(items: List[EvmWord] = List.empty) {
  import EvmStack._

  if (items.length > MaxSize) throw new StackOverflowException(s"stack exceeded maximum size of $MaxSize")

  def push[A](a: A)(implicit c: A => EvmWord) = Try(EvmStack((a: EvmWord) +: items))
  def push(hnil: HNil) = Try(this)
  // Push an HList on the stack (right pushed first)
  def push[L <: HList](l: L)(implicit f: RightFolder.Aux[L, Try[EvmStack], pushWord.type, Try[EvmStack]]): Try[EvmStack] =
    l.foldRight(Try(this))(pushWord)

  //def pushP(word: EvmWord) = Try(word +: items)
  def popP = Try(items.headOption.map(w => (w, EvmStack(items.tail))).getOrElse(throw new StackUnderflowException))

  def pop[A](toA: EvmWord => A): Try[(A :: HNil, EvmStack)] =
    Try(items.headOption.map(w => (HList(toA(w)), EvmStack(items.tail))).getOrElse(throw new StackUnderflowException))
  // Pop an HList off the stack (left popped first)
  def pop[L <: HList](implicit p: Pop[L]) = p.pop(this)


//  def pushT[P <: Product](p: P)(implicit f: RightFolderT.Aux[P, Try[EvmStack], pushWord.type, Try[EvmStack]]): Try[EvmStack] =
//    p.foldRight(Try(this))(pushWord)
//  def pushT(u: Unit) = Try(this)
//
//  def popT[A](toA: EvmWord => A): Try[(A, EvmStack)] =
//    Try(items.headOption.map(w => (toA(w), EvmStack(items.tail))).getOrElse(throw new StackUnderflowException))
//  def popT[P <: Product](implicit p: PopT[P]) = p.pop(this)


}

object EvmStack {

  type Push[L <: HList] = RightFolder.Aux[L, Try[EvmStack], pushWord.type, Try[EvmStack]]

  val MaxSize = 1024

  val empty = EvmStack()


  class StackOverflowException(msg: String) extends EvmException(msg)
  class StackUnderflowException extends EvmException


  // EVM word conversions
  object Conversions {

    // BigInteger
    implicit def wordToBigInteger(word: EvmWord) = new BigInteger(word)
    implicit def bigIntegerToWord(value: BigInteger): EvmWord = value.toByteArray

  }

  // Polymorphic function that allows an HList to be pushed on the stack
  object pushWord extends Poly2 {
    implicit def default[A](implicit c: A => EvmWord) = at[A, Try[EvmStack]]((a, s) => s.flatMap(_.push(a)))
  }

  // Type class that implements a generic HList pop operation
  // Inspiration: https://meta.plasm.us/posts/2013/06/09/learning-shapeless/
  sealed trait Pop[L <: HList] {
    def pop(s: EvmStack): Try[(L, EvmStack)]
  }

  implicit object hnilPop extends Pop[HNil] { override def pop(s: EvmStack) = Try((HNil, s)) }

  implicit def hlistPop[L <: HList, H, T <: HList](
    implicit
    hCons: IsHCons.Aux[L, H, T],
    toH: EvmWord => H, tp: Pop[T]
  ) = new Pop[H :: T] {
    override def pop(s: EvmStack): Try[(H :: T, EvmStack)] = for {
      (h, hs) <- s.pop[H](toH)
      (t, ts) <- tp.pop(hs)
    } yield (h ++ t, ts)
  }



//  sealed trait PopT[P] {
//    def pop(s: EvmStack): Try[(P, EvmStack)]
//  }
//
////  implicit object unitPop extends PopT[Unit] { override def pop(s: EvmStack) = Try(((), s)) }
//  implicit object unitPopT extends PopT[Product1[Unit]] { override def pop(s: EvmStack) = Try((Tuple1(()), s)) }

//  implicit def singlePop[T](
//    implicit
//    toH: EvmWord => T
//  ) = new PopT[T] {
//    override def pop(s: EvmStack): Try[(T, EvmStack)] = s.popT[T](toH)
//  }
//val n: Int = HNil.tupled

//  implicit def singlePop[T](
//    implicit
//    toH: EvmWord => T
//  ) = new PopT[Tuple1[T]] {
//    override def pop(s: EvmStack): Try[(Tuple1[T], EvmStack)] = s.popT[T](toH).map[(Tuple1[T], EvmStack)] { case(t, s) => (Tuple1(t), s) }
//  }

//  implicit def tuplePop[P <: Product, H0, T0](
//    implicit
//    isComp: IsComposite[P] { type H = H0; type T = T0},
////    isComp: IsComposite.Aux[P, H, T],// { type P = P; type H = H; type T = T},
//    prepend: Prepend.Aux[H0, T0, P] { type H = H0; type T = T0; type Out = P },
////    prepend2: Prepend[H0, T0] { type Out = P },
////    prepend: Prepend.Aux[H, T, P], // { type H = H; type T = T; type Out = P },
//    toH: EvmWord => H0,
//    tp: PopT[T0]
//  ) = new PopT[prepend.Out] {
//    override def pop(s: EvmStack): Try[(prepend.Out, EvmStack)] = for {
//      (h, hs) <- s.popT[prepend.H](toH)
//      (t, ts) <- tp.pop(hs)
//    } yield (prepend(h, t), ts)
//  }


}