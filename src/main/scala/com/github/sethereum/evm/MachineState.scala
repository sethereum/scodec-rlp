package com.github.sethereum.evm

import java.math.BigInteger

import shapeless.HNil
import shapeless.ops.hlist.RightFolder

import scala.language.higherKinds

import shapeless._
import scala.language.implicitConversions

import scala.util.Try

//import shapeless.HList.ListCompat._



case class StackItem(bytes: Array[Byte]) {
  require(bytes.length == 32, s"invalid stack item (length=${bytes.length})")
}

case class Stack(items: List[EvmWord] = List.empty) {
  import Stack._
  import shapeless.HList.ListCompat._

  def push[A](value: A)(implicit c: A => EvmWord): Stack = Stack((value: EvmWord) +: items)
  // Push an HList on the stack
  def push[L <: HList](l: L)(implicit f: RightFolder[L, Stack, pushWord.type]) = l.foldRight(this)(pushWord)

  // Polymorphic function that allows an HList to be pushed on the stack
  object pushWord extends Poly2 {
    implicit def default[A](implicit c: A => EvmWord) = at[A, Stack]((a, s) => s.push(a))
  }

  def pop[A](implicit c: EvmWord => A): (Option[A], Stack) = (items.headOption.map(c), Stack(items.tail))
  def pop[H, T <: HList]: (Option[H :: T], Stack)


  object popWord extends Poly2 {
    implicit def default[H, T <: HList](implicit c: EvmWord => H) =
      at[(Option[H :: T], Stack), EvmWord]{ case ((ht, s), w) => s.push(a) }
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