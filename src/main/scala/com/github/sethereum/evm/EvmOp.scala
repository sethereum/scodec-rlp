package com.github.sethereum.evm

import java.math.BigInteger
import com.github.sethereum.evm.EvmStateTransition._
import EvmWordConversions._

import scala.util.Try


/**
 * EVM operation represented as a state transition function.
 *
 * @param opcode
 * @param transition
 */
sealed abstract class EvmOp(opcode: Int)(transition: StateTransitionFunction) {
  def apply(state: EvmState) = transition(state)
}


object EvmOp {
  import Function._

//  case object STOP          extends EvmStackOperation[HNil, HNil]                   (0x00)  (identity)
  case object ADD           extends EvmOp   (0x01)  (add)
  case object MUL           extends EvmOp   (0x02)  (mul)
  case object SUB           extends EvmOp   (0x03)  (sub)
  case object DIV           extends EvmOp   (0x04)  (div)
//  case object SDIV          extends OpCode[BigInteger_2, BigInteger_1]   (0x05)
//  case object MOD           extends OpCode[BigInteger_3, BigInteger_1]   (0x06)
//  case object SMOD          extends OpCode[BigInteger_3, BigInteger_1]   (0x07)
//  case object ADDMOD        extends OpCode[BigInteger_3, BigInteger_1]   (0x08)
//  case object MULMOD        extends OpCode[BigInteger_3, BigInteger_1]   (0x09)
//  case object EXP           extends OpCode[BigInteger_2, BigInteger_1]   (0x0a)
//  case object SIGNEXTEND    extends OpCode[BigInteger_2, BigInteger_1]   (0x0b)
//  case object LT            extends OpCode[BigInteger_2, BigInteger_1]   (0x10)
//  case object GT            extends OpCode[BigInteger_2, BigInteger_1]   (0x11)
//  case object SLT           extends OpCode[BigInteger_2, BigInteger_1]   (0x12)
//  case object SGT           extends OpCode[BigInteger_2, BigInteger_1]   (0x13)
//  case object EQ            extends OpCode[BigInteger_2, BigInteger_1]   (0x14)
//  case object ISZERO        extends OpCode[BigInteger_1, BigInteger_1]   (0x15)


  case object AND           extends EvmOp   (0x16)  (and)
  case object OR            extends EvmOp   (0x17)  (or )
  case object XOR           extends EvmOp   (0x18)  (xor)
  case object NOT           extends EvmOp   (0x19)  (not)


//  case object BYTE          extends OpCode[BigInteger_2, BigInteger_1]   (0x1a)
//  case object SHA3          extends OpCode[_2, _1]   (0x20)
//  case object ADDRESS       extends OpCode[_0, _1]   (0x30)
//  case object BALANCE       extends OpCode[_1, _1]   (0x31)
//  case object ORIGIN        extends OpCode[_0, _1]   (0x32)
//  case object CALLER        extends OpCode[_0, _1]   (0x33)
//  case object CALLVALUE     extends OpCode[_0, _1]   (0x34)
//  case object CALLDATALOAD  extends OpCode[_1, _1]   (0x35)
//  case object CALLDATASIZE  extends OpCode[_0, _1]   (0x36)
//  case object CALLDATACOPY  extends OpCode[_3, _0]   (0x37)
//  case object CODESIZE      extends OpCode[_0, _1]   (0x38)
//  case object CODECOPY      extends OpCode[_3, _0]   (0x39)
//  case object GASPRICE      extends OpCode[_0, _1]   (0x3a)
//  case object EXTCODESIZE   extends OpCode[_1, _1]   (0x3b)
//  case object EXTCODECOPY   extends OpCode[_4, _0]   (0x3c)
//  case object BLOCKHASH     extends OpCode[_1, _1]   (0x40)
//  case object COINBASE      extends OpCode[_0, _1]   (0x41)
//  case object TIMESTAMP     extends OpCode[_0, _1]   (0x42)
//  case object NUMBER        extends OpCode[_0, _1]   (0x43)
//  case object DIFFICULTY    extends OpCode[_0, _1]   (0x44)
//  case object GASLIMIT      extends OpCode[_0, _1]   (0x45)
//  case object POP           extends OpCode[_1, _0]   (0x50)
//  case object MLOAD         extends OpCode[_1, _1]   (0x51)
//  case object MSTORE        extends OpCode[_2, _0]   (0x52)
//  case object MSTORE8       extends OpCode[_2, _0]   (0x53)
//  case object SLOAD         extends EvmOperation[Key :: HNil, Value :: HNil]   (0x54) ({ case (k :: HNil, state) => (HList(state.storage(k)), state) })
//  case object SSTORE        extends EvmOperation[Key :: Value :: HNil, HNil]   (0x55) ({ case (k :: v :: HNil, state) => (HNil, EvmState.storageLens.modify(state)(_ + (k -> v))) })
//  case object JUMP          extends OpCode[_1, _0]   (0x56)
//  case object JUMPI         extends OpCode[_2, _0]   (0x57)
//  case object PC            extends OpCode[_0, _1]   (0x58)
//  case object MSIZE         extends OpCode[_0, _1]   (0x59)
//  case object GAS           extends OpCode[_0, _1]   (0x5a)
//  case object JUMPDEST      extends OpCode[_0, _0]   (0x5b)
//  case object PUSH1         extends OpCode[_0, _1]   (0x60)
//  case object PUSH2         extends OpCode[_0, _1]   (0x61)
//  case object PUSH3         extends OpCode[_0, _1]   (0x62)
//  case object PUSH4         extends OpCode[_0, _1]   (0x63)
//  case object PUSH5         extends OpCode[_0, _1]   (0x64)
//  case object PUSH6         extends OpCode[_0, _1]   (0x65)
//  case object PUSH7         extends OpCode[_0, _1]   (0x66)
//  case object PUSH8         extends OpCode[_0, _1]   (0x67)
//  case object PUSH9         extends OpCode[_0, _1]   (0x68)
//  case object PUSH10        extends OpCode[_0, _1]   (0x69)
//  case object PUSH11        extends OpCode[_0, _1]   (0x6a)
//  case object PUSH12        extends OpCode[_0, _1]   (0x6b)
//  case object PUSH13        extends OpCode[_0, _1]   (0x6c)
//  case object PUSH14        extends OpCode[_0, _1]   (0x6d)
//  case object PUSH15        extends OpCode[_0, _1]   (0x6e)
//  case object PUSH16        extends OpCode[_0, _1]   (0x6f)
//  case object PUSH17        extends OpCode[_0, _1]   (0x70)
//  case object PUSH18        extends OpCode[_0, _1]   (0x71)
//  case object PUSH19        extends OpCode[_0, _1]   (0x72)
//  case object PUSH20        extends OpCode[_0, _1]   (0x73)
//  case object PUSH21        extends OpCode[_0, _1]   (0x74)
//  case object PUSH22        extends OpCode[_0, _1]   (0x75)
//  case object PUSH23        extends OpCode[_0, _1]   (0x76)
//  case object PUSH24        extends OpCode[_0, _1]   (0x77)
//  case object PUSH25        extends OpCode[_0, _1]   (0x78)
//  case object PUSH26        extends OpCode[_0, _1]   (0x79)
//  case object PUSH27        extends OpCode[_0, _1]   (0x7a)
//  case object PUSH28        extends OpCode[_0, _1]   (0x7b)
//  case object PUSH29        extends OpCode[_0, _1]   (0x7c)
//  case object PUSH30        extends OpCode[_0, _1]   (0x7d)
//  case object PUSH31        extends OpCode[_0, _1]   (0x7e)
//  case object PUSH32        extends OpCode[_0, _1]   (0x7f)
//  case object DUP1          extends OpCode[_1, _2]   (0x80)
//  case object DUP2          extends OpCode[_2, _3]   (0x81)
//  case object DUP3          extends OpCode[_3, _4]   (0x82)
//  case object DUP4          extends OpCode[_4, _5]   (0x83)
//  case object DUP5          extends OpCode[_5, _6]   (0x84)
//  case object DUP6          extends OpCode[_6, _7]   (0x85)
//  case object DUP7          extends OpCode[_7, _8]   (0x86)
//  case object DUP8          extends OpCode[_8, _9]   (0x87)
//  case object DUP9          extends OpCode[_9, _10]  (0x88)
//  case object DUP10         extends OpCode[_10, _11] (0x89)
//  case object DUP11         extends OpCode[_11, _12] (0x8a)
//  case object DUP12         extends OpCode[_12, _13] (0x8b)
//  case object DUP13         extends OpCode[_13, _14] (0x8c)
//  case object DUP14         extends OpCode[_14, _15] (0x8d)
//  case object DUP15         extends OpCode[_15, _16] (0x8e)
//  case object DUP16         extends OpCode[_16, _17] (0x8f)
//  case object SWAP1         extends OpCode[_2, _2]   (0x90)
//  case object SWAP2         extends OpCode[_3, _3]   (0x91)
//  case object SWAP3         extends OpCode[_4, _4]   (0x92)
//  case object SWAP4         extends OpCode[_5, _5]   (0x93)
//  case object SWAP5         extends OpCode[_6, _6]   (0x94)
//  case object SWAP6         extends OpCode[_7, _7]   (0x95)
//  case object SWAP7         extends OpCode[_8, _8]   (0x96)
//  case object SWAP8         extends OpCode[_9, _9]   (0x97)
//  case object SWAP9         extends OpCode[_10, _10] (0x98)
//  case object SWAP10        extends OpCode[_11, _11] (0x99)
//  case object SWAP11        extends OpCode[_12, _12] (0x9a)
//  case object SWAP12        extends OpCode[_13, _13] (0x9b)
//  case object SWAP13        extends OpCode[_14, _14] (0x9c)
//  case object SWAP14        extends OpCode[_15, _15] (0x9d)
//  case object SWAP15        extends OpCode[_16, _16] (0x9e)
//  case object SWAP16        extends OpCode[_17, _17] (0x9f)
//  case object LOG0          extends OpCode[_2, _0]   (0xa0)
//  case object LOG1          extends OpCode[_3, _0]   (0xa1)
//  case object LOG2          extends OpCode[_4, _0]   (0xa2)
//  case object LOG3          extends OpCode[_5, _0]   (0xa3)
//  case object LOG4          extends OpCode[_6, _0]   (0xa4)


  object Function {
    import BigInteger._

    val MOD256 = ONE.shiftLeft(256)

    def add = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.add(y)) }
    def mul = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.multiply(y)) }
    def sub = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.subtract(y)) }
    def div = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(if (y.equals(ZERO)) ZERO else x.divide(y)) }

    def and = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.and(y)) }
    def or  = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.or(y)) }
    def xor = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.xor(y)) }
    def not = (x: BigInteger) =>                    (state: EvmState) => { state.push(x.not()) }

  }

}

