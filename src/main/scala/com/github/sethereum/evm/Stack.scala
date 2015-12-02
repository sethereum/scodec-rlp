package com.github.sethereum.evm

import shapeless.{::, HNil, Poly2, HList}
import shapeless.ops.hlist.{IsHCons, RightFolder}

case class Stack(items: List[EvmWord] = List.empty) {
  import Stack._

  def push[A](a: A)(implicit c: A => EvmWord): Stack = Stack((a: EvmWord) +: items)
  // Push an HList on the stack (right pushed first)
  def push[L <: HList](l: L)(implicit f: RightFolder[L, Stack, pushWord.type]) = l.foldRight(this)(pushWord)

  def pop[A <: HNil] = Some((HNil, this))
  def pop[A](implicit toA: EvmWord => A): Option[(A :: HNil, Stack)] =
    items.headOption.map(w => (HList(toA(w)), Stack(items.tail)))

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

object Stack {

  // Polymorphic function that allows an HList to be pushed on the stack
  private [evm] object pushWord extends Poly2 {
    implicit def default[A](implicit c: A => EvmWord) = at[A, Stack]((a, s) => s.push(a))
  }

  // Type class and associated implementation for generic HList pop operation
  // See: https://meta.plasm.us/posts/2013/06/09/learning-shapeless/
  private [evm] sealed trait Pop[L <: HList] {
    def pop(s: Stack): Option[(L, Stack)]
  }

  private [evm] implicit object hnilPop extends Pop[HNil] { override def pop(s: Stack) = Some((HNil, s)) }

  private [evm] implicit def hlistPop[L <: HList, H, T <: HList](
    implicit
    hCons: IsHCons.Aux[L, H, T],
    toH: EvmWord => H, tp: Pop[T]
  ) = new Pop[H :: T] {
    override def pop(s: Stack): Option[(H :: T, Stack)] = for {
      (h, hs) <- s.pop[H](toH)
      (t, ts) <- tp.pop(hs)
    } yield (h ++ t, ts)
  }

}