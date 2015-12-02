package com.github.sethereum.evm

import java.math.BigInteger

import shapeless.ops.hlist.{IsHCons, RightFolder}
import shapeless.{::, HList, HNil, Poly2}

import scala.language.implicitConversions

/**
 * Immutable stack implementation for the EVM.
 *
 * Type safe push/pop operations are provided via an HList interface.
 *
 * @param items
 */
case class EvmStack(items: List[EvmWord] = List.empty) {
  import EvmStack._

  if (items.length > MaxSize) throw new StackOverflowException

  def push[A](a: A)(implicit c: A => EvmWord): EvmStack = EvmStack((a: EvmWord) +: items)
  def push(hnil: HNil) = this
  // Push an HList on the stack (right pushed first)
//  def push[L <: HList, RF <: RightFolder[L, EvmStack, pushWord.type]](l: L)(implicit f: RF): RF#Out = l.foldRight(this)(pushWord)
  def push[L <: HList](l: L)(implicit f: RightFolder.Aux[L, EvmStack, pushWord.type, EvmStack]): EvmStack = l.foldRight(this)(pushWord)

  //def pop[A <: HNil] = Some((HNil, this))
  def pop[A](toA: EvmWord => A): Option[(A :: HNil, EvmStack)] =
    items.headOption.map(w => (HList(toA(w)), EvmStack(items.tail)))

  // Pop an HList off the stack (left popped first)
  def pop[L <: HList](implicit p: Pop[L]) = p.pop(this)

  /*
    def pop[A, B](
        implicit toA: EvmWord => A, toB: EvmWord => B
      ): Option[(A :: B :: HNil, Stack)] = for {
        (a, sa) <- pop[A](toA)
        (b, sb) <- sa.pop[B](toB)
      } yield (a ++ b, sb)

    def pop[A, B, C](
        implicit toA: EvmWord => A, toB: EvmWord => B, toC: EvmWord => C
      ): Option[(A :: B :: C:: HNil, Stack)] = for {
        (ab, sab) <- pop[A, B](toA, toB)
        (c, sc) <- sab.pop[C](toC)
      } yield (ab ++ c, sc)

    def pop[A, B, C, D](
        implicit toA: EvmWord => A, toB: EvmWord => B, toC: EvmWord => C, toD: EvmWord => D
      ): Option[(A :: B :: C:: D :: HNil, Stack)] = for {
        (abc, sabc) <- pop[A, B, C](toA, toB, toC)
        (d, sd) <- sabc.pop[D](toD)
      } yield (abc ++ d, sd)

    def pop[A, B, C, D, E](
        implicit toA: EvmWord => A, toB: EvmWord => B, toC: EvmWord => C, toD: EvmWord => D, toE: EvmWord => E
      ): Option[(A :: B :: C:: D :: E :: HNil, Stack)] = for {
        (abcd, sabcd) <- pop[A, B, C, D](toA, toB, toC, toD)
        (e, se) <- sabcd.pop[E](toE)
      } yield (abcd ++ e, se)
  */

}

object EvmStack {

  val MaxSize = 1024

  val empty = EvmStack()

  class StackOverflowException extends EvmException(s"stack exceeded maximum size of $MaxSize")
  class StackUnderflowException extends EvmException

  // EVM word conversions
  object Conversions {

    // BigInteger
    implicit def wordToBigInteger(word: EvmWord) = new BigInteger(word)
    implicit def bigIntegerToWord(value: BigInteger): EvmWord = value.toByteArray

  }

  // Polymorphic function that allows an HList to be pushed on the stack
  private [evm] object pushWord extends Poly2 {
    implicit def default[A](implicit c: A => EvmWord) = at[A, EvmStack]((a, s) => s.push(a))
  }

  // Type class that implements a generic HList pop operation
  // Inspiration: https://meta.plasm.us/posts/2013/06/09/learning-shapeless/
  private [evm] sealed trait Pop[L <: HList] {
    def pop(s: EvmStack): Option[(L, EvmStack)]
  }

  private [evm] implicit object hnilPop extends Pop[HNil] { override def pop(s: EvmStack) = Some((HNil, s)) }

  private [evm] implicit def hlistPop[L <: HList, H, T <: HList](
    implicit
    hCons: IsHCons.Aux[L, H, T],
    toH: EvmWord => H, tp: Pop[T]
  ) = new Pop[H :: T] {
    override def pop(s: EvmStack): Option[(H :: T, EvmStack)] = for {
      (h, hs) <- s.pop[H](toH)
      (t, ts) <- tp.pop(hs)
    } yield (h ++ t, ts)
  }

}