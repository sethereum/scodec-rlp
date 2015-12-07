package com.github.sethereum.evm

import com.github.sethereum.evm.EvmStateTransition.StateTransitionFunction

import scala.language.implicitConversions
import scala.util.Try

sealed trait EvmStateTransition[F] {
  def apply(f: F): StateTransitionFunction
}

object EvmStateTransition {

  /**
   * Represents functions that perform a state transition.
   */
  type StateTransitionFunction = EvmState => Try[EvmState]

  // Represents state transitions that take no arguments
  implicit object NoArgStateTransition extends EvmStateTransition[StateTransitionFunction] {
    override def apply(f: StateTransitionFunction) = f
  }

  /**
   * Implicit conversion for state transitions that accept arguments arguments.
   *
   * Will chain recursively to support functions taking an arbitrary number of arguments
   * Ex: def add = (x: BigInteger) => (y: BigInteger) => (state: EvmState2) => { state.push(x.add(y)) }
   */
  implicit def withArgsStateTransition[A, B, C](
    implicit
    fromW: EvmWord => A,
    f: EvmStateTransition[B => C]
  ) = new EvmStateTransition[A => B => C] {
    override def apply(g: A => B => C): StateTransitionFunction =  { state: EvmState =>
      for {
        (a, sta) <- state.pop[A]
        stf <- f(g(a))(sta)
      } yield stf
    }
  }

  // Implicit conversion of an ordinary function into a state transition function
  implicit def stateTransitionFunc[A, B](f: A => B)(implicit t: EvmStateTransition[A => B]): StateTransitionFunction =
    t.apply(f)

}