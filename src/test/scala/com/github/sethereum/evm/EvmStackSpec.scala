package com.github.sethereum.evm

import java.math.BigInteger

import com.github.sethereum.evm.EvmStack.Conversions._
import com.github.sethereum.evm.EvmStack._
import com.github.sethereum.evm.EvmStack.pushWord._
import org.scalatest.{Matchers, TryValues, WordSpec}
import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist.{IsHCons, RightFolder}

import scala.language.{implicitConversions, higherKinds}
import scala.util.{Success, Try}

class EvmStackSpec extends WordSpec with Matchers with TryValues {

  "EVM stack" should {

    "push/pop HNil" in {

      val stack = EvmStack()
      stack.push(HNil) shouldBe Success(stack)

      stack.pop[HNil] shouldBe Success((HNil), stack)
    }

    "push/pop HList" in {
      val expected = EvmStack(List((BigInteger.ZERO: EvmWord)))

      type Items = BigInteger :: HNil
      val hlist: Items = BigInteger.ZERO :: HNil
      val actual = EvmStack().push(hlist)

//      actual.items.length shouldBe expected.items.length
//      actual.items.zip(expected.items).foreach { case (a, e) => a should === (e) }

      //actual.pop[Items] shouldBe Some((hlist, EvmStack.empty))

      actual.success.value.items.length shouldBe expected.items.length
      actual.success.value.items.zip(expected.items).foreach { case (a, e) => a should === (e) }

      actual.success.value.pop[Items] shouldBe Success((hlist, EvmStack.empty))
    }


    import shapeless.syntax.std.function._


    // Transform an arbitrary function to a stack-based function (stack as single argument)
    // Pop all arguments from stack, performing conversion
    // Call function
    // Push results onto stack
    sealed abstract class StackOp[F, L <: HList, R, RL <: HList](toRL: R => RL)(implicit fp: FnToProduct.Aux[F, L => R], pop: Pop[L], rf: RightFolder.Aux[RL, Try[EvmStack], pushWord.type, Try[EvmStack]]) {
      def apply(f: F)(state: EvmState): Try[EvmState] = for {
        (args, sargs) <- pop.pop(state.stack)
        sout <- sargs.push(toRL(f.toProduct(args)))
      } yield EvmState.stackLens.set(state)(sout)
    }


    implicit def unitStackOp[F, L <: HList, R <: Unit](implicit fp: FnToProduct.Aux[F, L => R], pop: Pop[L], rf: RightFolder.Aux[HNil, Try[EvmStack], pushWord.type, Try[EvmStack]]) =
      new StackOp[F, L, R, HNil](r => HNil){ }

    implicit def singleStackOp[F, L <: HList, R](implicit fp: FnToProduct.Aux[F, L => R], pop: Pop[L], rf: RightFolder.Aux[R :: HNil, Try[EvmStack], pushWord.type, Try[EvmStack]]) =
      new StackOp[F, L, R, R :: HNil](r => HList(r)){ }

    implicit def productStackOp[F, L <: HList, R <: Product, RL <: HList](implicit fp: FnToProduct.Aux[F, L => R], pop: Pop[L], gen: Generic.Aux[R, RL], rf: RightFolder.Aux[RL, Try[EvmStack], pushWord.type, Try[EvmStack]]) =
      new StackOp[F, L, R, RL](r => gen.to(r)){ }

    def getStackOp[F, L <: HList, R, RL <: HList](f: F)(implicit op: StackOp[F, L, R, RL]) = op


    // Convert a function of type (Args, State) => Try[(Result, State)] into State => Try[State]

    sealed trait EvmStateTransition {
      def apply(state: EvmState): Try[EvmState]
    }

//    implicit def unitStackStateTransition[F, L <: HList, R <: Unit]

    // Type class that implements a generic HList pop operation
    // Inspiration: https://meta.plasm.us/posts/2013/06/09/learning-shapeless/
    sealed trait PopList[L <: HList] {
      def apply(s: EvmState): Try[(L, EvmState)]
    }

    implicit object hnilPopList extends PopList[HNil] { override def apply(s: EvmState) = Try((HNil, s)) }

    implicit def hlistPopList[L <: HList, H, T <: HList](
      implicit
      hCons: IsHCons.Aux[L, H, T],
      toH: EvmWord => H, tp: PopList[T]
    ) = new PopList[H :: T] {
      override def apply(s: EvmState): Try[(H :: T, EvmState)] = for {
        (h, hs) <- s.stack.popP.map { case (w, s) => (HList(toH(w)), s) }
        (t, ts) <- tp(EvmState.stackLens.set(s)(hs))
      } yield (h ++ t, ts)
    }

    case class PopArguments[F, L <: HList, R](f: F)(implicit fp: FnToProduct.Aux[F, L => R], pop: PopList[L]) {
      def apply(state: EvmState) = pop(state)
    }


    sealed abstract class PushProduct[R, RL <: HList](toList: R => RL)(implicit rf: RightFolder.Aux[RL, Try[EvmStack], pushWord.type, Try[EvmStack]]) {
      def apply(result: (R, EvmState)): Try[EvmState] = result match {
        case (r, state) => for {
          sout <- state.stack.push(toList(r))
        } yield EvmState.stackLens.set(state)(sout)
      }
    }

    implicit def pushUnitProduct[F, L <: HList, R <: Unit](implicit fp: FnToProduct.Aux[F, L => R], rf: RightFolder.Aux[HNil, Try[EvmStack], pushWord.type, Try[EvmStack]]) =
      new PushProduct[R, HNil](r => HNil){ }

    implicit def pushSingleProduct[F, L <: HList, R](implicit fp: FnToProduct.Aux[F, L => R], rf: RightFolder.Aux[R :: HNil, Try[EvmStack], pushWord.type, Try[EvmStack]]) =
      new PushProduct[R, R :: HNil](r => HList(r)){ }

    implicit def pushCompositeProduct[F, L <: HList, R <: Product, RL <: HList](implicit fp: FnToProduct.Aux[F, L => R], gen: Generic.Aux[R, RL], rf: RightFolder.Aux[RL, Try[EvmStack], pushWord.type, Try[EvmStack]]) =
      new PushProduct[R, RL](r => gen.to(r)){ }


    case class PushResult[F, L <: HList, R, RL <: HList](f: F)(implicit fp: FnToProduct.Aux[F, L => R], push: PushProduct[R, RL]) {
      def apply(result: (R, EvmState)): Try[EvmState] = push(result)
    }


    type StateTransitionFunc = EvmState2 => Try[EvmState2]

    sealed trait StateTransitionType[F] {
      def apply(f: F): StateTransitionFunc
    }

    implicit object UnitStateTransition extends StateTransitionType[StateTransitionFunc] {
      override def apply(f: StateTransitionFunc) = f
    }

    implicit def argsStateTransition[A, B, F](implicit fromW: EvmWord => A, f: StateTransitionType[B => F]): StateTransitionType[A => B => F] =
      new StateTransitionType[A => B => F] {
        override def apply(g: A => B => F): StateTransitionFunc =  { state: EvmState2 =>
          for {
            (a, sta) <- state.pop[A]
            stf <- f(g(a))(sta)
          } yield stf
        }
      }

    implicit def stateTransitionFunc[A, B](f: A => B)(implicit t: StateTransitionType[A => B]): StateTransitionFunc = t.apply(f)

    val transition: StateTransitionFunc = stateTransitionFunc({ x: BigInteger => y: BigInteger => state: EvmState2 => state.push(x) })


    "math operation" in {



      def add(l: BigInteger, r: BigInteger): BigInteger = l.add(r)
      def push2(l: BigInteger, r: BigInteger): (BigInteger, BigInteger) = (l, r)
      def noop = ()

      val before = EvmStack(List(BigInteger.ONE, BigInteger.ONE).map(n => (n:EvmWord)))
      val expected = EvmStack(List(BigInteger.valueOf(2)).map(n => (n:EvmWord)))

      val stackNoopOp = getStackOp(noop _)
      val stackAddOp = getStackOp(add _)
      val stackPush2Op = getStackOp(push2 _)
    }

//    "push/pop Tuple" in {
//
//      val expected = EvmStack(List((BigInteger.ZERO: EvmWord), (BigInteger.ONE: EvmWord)))
//
//      type Items = (BigInteger, BigInteger)
//      val tuple: Items = (BigInteger.ZERO, BigInteger.ONE)
//      val actual = EvmStack().pushT(tuple)
//
//      //      actual.items.length shouldBe expected.items.length
//      //      actual.items.zip(expected.items).foreach { case (a, e) => a should === (e) }
//
//      //actual.pop[Items] shouldBe Some((hlist, EvmStack.empty))
//
//      actual.success.value.items.length shouldBe expected.items.length
//      actual.success.value.items.zip(expected.items).foreach { case (a, e) => a should === (e) }
//
////      actual.success.value.popT[Tuple1[BigInteger]] shouldBe Success((tuple, EvmStack.empty))
////      actual.success.value.popT[Tuple2[BigInteger, BigInteger]] shouldBe Success((tuple, EvmStack.empty))
////      actual.success.value.popT[(BigInteger, BigInteger)] shouldBe Success((tuple, EvmStack.empty))
//
//
//
//    }

  }

}
