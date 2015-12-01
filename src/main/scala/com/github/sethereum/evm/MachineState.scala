package com.github.sethereum.evm

import java.math.BigInteger

import shapeless.HNil
import shapeless.ops.hlist._

import scala.language.higherKinds

import shapeless._
import scala.language.implicitConversions

import scala.reflect.runtime.universe._



case class StackItem(bytes: Array[Byte]) {
  require(bytes.length == 32, s"invalid stack item (length=${bytes.length})")
}

case class Stack(items: List[EvmWord] = List.empty) {
  import Stack._
  import shapeless.HList.ListCompat._

  def push[A](a: A)(implicit c: A => EvmWord): Stack = Stack((a: EvmWord) +: items)
//  def push(hNil: HNil) = this
//  def push[H, T <: HList](list: H :: T)(implicit c: H => EvmWord): Stack = push(list.tail).push(list.head)(c)
//  def push[L <: HList](list: L): Stack = push(list.head :: list.tail)

  def pop[A](implicit c: EvmWord => A): (Option[A], Stack) = (items.headOption.map(c), Stack(items.tail))
//  def pop[T <: HNil] = (Some(HNil), this)
//  def pop[H, T <: HList] = 1
//  def pop[L <: HList](implicit hCons: IsHCons[L]) = pop[hCons.H, hCons.T]
  def pop[L <: HList, R <: HList](list: L)(
    implicit
    scanner: LeftScanner.Aux[L, (Option[_], Stack), popWord.type, R], 
    reducer: LeftReducer[R, combineOptions.type]
  ) = list.scanLeft((Some(HNil), this): (Option[_], Stack))(popWord).reduceLeft(combineOptions)


  type Operands = BigInteger :: HNil

  typeTag[Operands].tpe.etaExpand
  pop(typeTag[BigInteger] :: HNil)
//  pop[Operands]

  // Push an HList on the stack
  def push[L <: HList](l: L)(implicit f: RightFolder[L, Stack, pushWord.type]) = l.foldRight(this)(pushWord)

  // Polymorphic function that allows an HList to be pushed on the stack
  object pushWord extends Poly2 {
    implicit def default[A](implicit c: A => EvmWord) = at[A, Stack]((a, s) => s.push(a))
  }

  //def pop[H, T <: HList]: (Option[H :: T], Stack)


  object popWord extends Poly2 {
//    def genericPop[A](c: EvmWord => A) = at[(Option[_], Stack), A] { case ((_, s), a) => s.pop[A](c) }
//    implicit def default[A](implicit c: EvmWord => A) = at[(Option[_], Stack), A] { case ((_, s), a) => s.pop[A] }
    implicit def caseBigInteger(implicit c: EvmWord => BigInteger) = at[(Option[_], Stack), TypeTag[BigInteger]] { case ((_, s), a) => s.pop[BigInteger] }
//    implicit def default[H, T <: HList](implicit c: EvmWord => H) =
//      at[(Option[H :: T], Stack), EvmWord]{ case ((ht, s), w) => s.push(a) }
  }

  object combineOptions extends Poly1 {
    implicit def default[A, B] = at[((Option[A], Stack), (Option[B], Stack))] { case ((optA, _), (optB, s)) =>
      optA.map(a => optB.map(b => (a :: b :: HNil, s))).getOrElse((None, s))
    }
  }

  //  def pop[I]: (I :: HNil, Stack) = items match {
//    case (i: Item[I]) :: tail => (i.value :: HNil, Stack(tail))
//  }

}

//object Stack {
//
//  case class Item(bytes: Array[Byte])
//
//  object ItemConversions {
//    implicit def itemToBigInteger(item: Item): BigInteger = new BigInteger(item.bytes)
//    implicit def bigIntegerToItem(value: BigInteger): Item = Item(value.toByteArray)
//  }
//
//}


case class MachineState(stack: Seq[StackItem])