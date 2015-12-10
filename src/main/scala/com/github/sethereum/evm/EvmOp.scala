package com.github.sethereum.evm

import java.math.BigInteger

import com.github.sethereum.evm.EvmStateTransition._
import com.github.sethereum.evm.EvmWordConversions._

import scala.util.Try


/**
 * EVM operation represented as a state transition function.
 *
 * @param opcode
 * @param transition
 */
sealed abstract class EvmOp(val opcode: Int)(val pop: Int, push: Int)(val transition: StateTransitionFunction) {
  def apply(state: EvmState) = transition(state)
}

sealed abstract class EvmPushOp(val size: Int)(opcode: Int)(pop: Int, push: Int)(transition: StateTransitionFunction)
  extends EvmOp(opcode)(pop, push)(transition)
{
  override def apply(state: EvmState): Try[EvmState] = super.apply(state)
}

object EvmPushOp {
  def transition(size: Int)(state: EvmState) = {

  }
}


object EvmOp {
  import Function._

  case object STOP          extends EvmOp   (0x00)  (0, 0)    (stop        )
  case object ADD           extends EvmOp   (0x01)  (2, 1)    (add         )
  case object MUL           extends EvmOp   (0x02)  (2, 1)    (mul         )
  case object SUB           extends EvmOp   (0x03)  (2, 1)    (sub         )
  case object DIV           extends EvmOp   (0x04)  (2, 1)    (div         )
//  case object SDIV          extends EvmOp   (0x05)  (2, 1)    (sdiv        )
//  case object MOD           extends EvmOp   (0x06)  (3, 1)    (mod         )
//  case object SMOD          extends EvmOp   (0x07)  (3, 1)    (smod        )
//  case object ADDMOD        extends EvmOp   (0x08)  (3, 1)    (addmod      )
//  case object MULMOD        extends EvmOp   (0x09)  (3, 1)    (mulmod      )
//  case object EXP           extends EvmOp   (0x0a)  (2, 1)    (exp         )
//  case object SIGNEXTEND    extends EvmOp   (0x0b)  (2, 1)    (signextend  )
//  case object LT            extends EvmOp   (0x10)  (2, 1)    (lt          )
//  case object GT            extends EvmOp   (0x11)  (2, 1)    (gt          )
//  case object SLT           extends EvmOp   (0x12)  (2, 1)    (slt         )
//  case object SGT           extends EvmOp   (0x13)  (2, 1)    (sgt         )
//  case object EQ            extends EvmOp   (0x14)  (2, 1)    (eq          )
  case object ISZERO        extends EvmOp   (0x15)  (1, 1)    (iszero      )
  case object AND           extends EvmOp   (0x16)  (2, 1)    (and         )
  case object OR            extends EvmOp   (0x17)  (2, 1)    (or          )
  case object XOR           extends EvmOp   (0x18)  (2, 1)    (xor         )
  case object NOT           extends EvmOp   (0x19)  (1, 1)    (not         )
//  case object BYTE          extends EvmOp   (0x1a)  (2, 1)    (byte        )
//  case object SHA3          extends EvmOp   (0x20)  (2, 1)    (sha3        )
  case object ADDRESS       extends EvmOp   (0x30)  (0, 1)    (address     )
//  case object BALANCE       extends EvmOp   (0x31)  (1, 1)    (balance     )
  case object ORIGIN        extends EvmOp   (0x32)  (0, 1)    (origin      )
  case object CALLER        extends EvmOp   (0x33)  (0, 1)    (caller      )
//  case object CALLVALUE     extends EvmOp   (0x34)  (0, 1)    (callvalue   )
//  case object CALLDATALOAD  extends EvmOp   (0x35)  (1, 1)    (calldataload)
//  case object CALLDATASIZE  extends EvmOp   (0x36)  (0, 1)    (calldatasize)
//  case object CALLDATACOPY  extends EvmOp   (0x37)  (3, 0)    (calldatacopy)
//  case object CODESIZE      extends EvmOp   (0x38)  (0, 1)    (codesize    )
//  case object CODECOPY      extends EvmOp   (0x39)  (3, 0)    (codecopy    )
//  case object GASPRICE      extends EvmOp   (0x3a)  (0, 1)    (gasprice    )
//  case object EXTCODESIZE   extends EvmOp   (0x3b)  (1, 1)    (extcodesize )
//  case object EXTCODECOPY   extends EvmOp   (0x3c)  (4, 0)    (extcodecopy )
//  case object BLOCKHASH     extends EvmOp   (0x40)  (1, 1)    (blockhash   )
//  case object COINBASE      extends EvmOp   (0x41)  (0, 1)    (coinbase    )
//  case object TIMESTAMP     extends EvmOp   (0x42)  (0, 1)    (timestamp   )
//  case object NUMBER        extends EvmOp   (0x43)  (0, 1)    (number      )
//  case object DIFFICULTY    extends EvmOp   (0x44)  (0, 1)    (difficulty  )
//  case object GASLIMIT      extends EvmOp   (0x45)  (0, 1)    (gaslimit    )
  case object POP           extends EvmOp   (0x50)  (1, 0)    (pop         )
  case object MLOAD         extends EvmOp   (0x51)  (1, 1)    (mload       )
  case object MSTORE        extends EvmOp   (0x52)  (2, 0)    (mstore      )
  case object MSTORE8       extends EvmOp   (0x53)  (2, 0)    (mstore8     )
  case object SLOAD         extends EvmOp   (0x54)  (1, 1)    (sload       )
  case object SSTORE        extends EvmOp   (0x55)  (2, 0)    (sstore      )
//  case object JUMP          extends EvmOp   (0x56)  (1, 0)    (jump        )
//  case object JUMPI         extends EvmOp   (0x57)  (2, 0)    (jumpi       )
  case object PC            extends EvmOp   (0x58)  (0, 1)    (pc          )
  case object MSIZE         extends EvmOp   (0x59)  (0, 1)    (msize       )
//  case object GAS           extends EvmOp   (0x5a)  (0, 1)    (gas         )
//  case object JUMPDEST      extends EvmOp   (0x5b)  (0, 0)    (jumpdest    )
//  case object PUSH1         extends EvmOp   (0x60)  (0, 1)    (push1       )
//  case object PUSH2         extends EvmOp   (0x61)  (0, 1)    (push2       )
//  case object PUSH3         extends EvmOp   (0x62)  (0, 1)    (push3       )
//  case object PUSH4         extends EvmOp   (0x63)  (0, 1)    (push4       )
//  case object PUSH5         extends EvmOp   (0x64)  (0, 1)    (push5       )
//  case object PUSH6         extends EvmOp   (0x65)  (0, 1)    (push6       )
//  case object PUSH7         extends EvmOp   (0x66)  (0, 1)    (push7       )
//  case object PUSH8         extends EvmOp   (0x67)  (0, 1)    (push8       )
//  case object PUSH9         extends EvmOp   (0x68)  (0, 1)    (push9       )
//  case object PUSH10        extends EvmOp   (0x69)  (0, 1)    (push10      )
//  case object PUSH11        extends EvmOp   (0x6a)  (0, 1)    (push11      )
//  case object PUSH12        extends EvmOp   (0x6b)  (0, 1)    (push12      )
//  case object PUSH13        extends EvmOp   (0x6c)  (0, 1)    (push13      )
//  case object PUSH14        extends EvmOp   (0x6d)  (0, 1)    (push14      )
//  case object PUSH15        extends EvmOp   (0x6e)  (0, 1)    (push15      )
//  case object PUSH16        extends EvmOp   (0x6f)  (0, 1)    (push16      )
//  case object PUSH17        extends EvmOp   (0x70)  (0, 1)    (push17      )
//  case object PUSH18        extends EvmOp   (0x71)  (0, 1)    (push18      )
//  case object PUSH19        extends EvmOp   (0x72)  (0, 1)    (push19      )
//  case object PUSH20        extends EvmOp   (0x73)  (0, 1)    (push20      )
//  case object PUSH21        extends EvmOp   (0x74)  (0, 1)    (push21      )
//  case object PUSH22        extends EvmOp   (0x75)  (0, 1)    (push22      )
//  case object PUSH23        extends EvmOp   (0x76)  (0, 1)    (push23      )
//  case object PUSH24        extends EvmOp   (0x77)  (0, 1)    (push24      )
//  case object PUSH25        extends EvmOp   (0x78)  (0, 1)    (push25      )
//  case object PUSH26        extends EvmOp   (0x79)  (0, 1)    (push26      )
//  case object PUSH27        extends EvmOp   (0x7a)  (0, 1)    (push27      )
//  case object PUSH28        extends EvmOp   (0x7b)  (0, 1)    (push28      )
//  case object PUSH29        extends EvmOp   (0x7c)  (0, 1)    (push29      )
//  case object PUSH30        extends EvmOp   (0x7d)  (0, 1)    (push30      )
//  case object PUSH31        extends EvmOp   (0x7e)  (0, 1)    (push31      )
//  case object PUSH32        extends EvmOp   (0x7f)  (0, 1)    (push32      )
//  case object DUP1          extends EvmOp   (0x80)  (1, 2)    (dup1        )
//  case object DUP2          extends EvmOp   (0x81)  (2, 3)    (dup2        )
//  case object DUP3          extends EvmOp   (0x82)  (3, 4)    (dup3        )
//  case object DUP4          extends EvmOp   (0x83)  (4, 5)    (dup4        )
//  case object DUP5          extends EvmOp   (0x84)  (5, 6)    (dup5        )
//  case object DUP6          extends EvmOp   (0x85)  (6, 7)    (dup6        )
//  case object DUP7          extends EvmOp   (0x86)  (7, 8)    (dup7        )
//  case object DUP8          extends EvmOp   (0x87)  (8, 9)    (dup8        )
//  case object DUP9          extends EvmOp   (0x88)  (9, 10)   (dup9        )
//  case object DUP10         extends EvmOp   (0x89)  (10, 11)  (dup10       )
//  case object DUP11         extends EvmOp   (0x8a)  (11, 12)  (dup11       )
//  case object DUP12         extends EvmOp   (0x8b)  (12, 13)  (dup12       )
//  case object DUP13         extends EvmOp   (0x8c)  (13, 14)  (dup13       )
//  case object DUP14         extends EvmOp   (0x8d)  (14, 15)  (dup14       )
//  case object DUP15         extends EvmOp   (0x8e)  (15, 16)  (dup15       )
//  case object DUP16         extends EvmOp   (0x8f)  (16, 17)  (dup16       )
//  case object SWAP1         extends EvmOp   (0x90)  (2, 2)    (swap1       )
//  case object SWAP2         extends EvmOp   (0x91)  (3, 3)    (swap2       )
//  case object SWAP3         extends EvmOp   (0x92)  (4, 4)    (swap3       )
//  case object SWAP4         extends EvmOp   (0x93)  (5, 5)    (swap4       )
//  case object SWAP5         extends EvmOp   (0x94)  (6, 6)    (swap5       )
//  case object SWAP6         extends EvmOp   (0x95)  (7, 7)    (swap6       )
//  case object SWAP7         extends EvmOp   (0x96)  (8, 8)    (swap7       )
//  case object SWAP8         extends EvmOp   (0x97)  (9, 9)    (swap8       )
//  case object SWAP9         extends EvmOp   (0x98)  (10, 10)  (swap9       )
//  case object SWAP10        extends EvmOp   (0x99)  (11, 11)  (swap10      )
//  case object SWAP11        extends EvmOp   (0x9a)  (12, 12)  (swap11      )
//  case object SWAP12        extends EvmOp   (0x9b)  (13, 13)  (swap12      )
//  case object SWAP13        extends EvmOp   (0x9c)  (14, 14)  (swap13      )
//  case object SWAP14        extends EvmOp   (0x9d)  (15, 15)  (swap14      )
//  case object SWAP15        extends EvmOp   (0x9e)  (16, 16)  (swap15      )
//  case object SWAP16        extends EvmOp   (0x9f)  (17, 17)  (swap16      )
//  case object LOG0          extends EvmOp   (0xa0)  (2, 0)    (log0        )
//  case object LOG1          extends EvmOp   (0xa1)  (3, 0)    (log1        )
//  case object LOG2          extends EvmOp   (0xa2)  (4, 0)    (log2        )
//  case object LOG3          extends EvmOp   (0xa3)  (5, 0)    (log3        )
//  case object LOG4          extends EvmOp   (0xa4)  (6, 0)    (log4        )


  object Function {
    import BigInteger._

    val MOD256 = ONE.shiftLeft(256)

    def stop = (state: EvmState) => { state.stop }
    def add    = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.add(y)) }
    def mul    = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.multiply(y)) }
    def sub    = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.subtract(y)) }
    def div    = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(if (y.equals(ZERO)) ZERO else x.divide(y)) }

    def iszero = (x: BigInteger) =>                    (state: EvmState) => { state.push(if (x == ZERO) ONE else ZERO) }
    def and    = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.and(y)) }
    def or     = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.or(y)) }
    def xor    = (x: BigInteger) => (y: BigInteger) => (state: EvmState) => { state.push(x.xor(y)) }
    def not    = (x: BigInteger) =>                    (state: EvmState) => { state.push(x.not()) }

    def address = (state: EvmState) => { state.push(state.environment.address) }
    def origin  = (state: EvmState) => { state.push(state.environment.origin) }
    def caller  = (state: EvmState) => { state.push(state.environment.caller) }

    def pop     = (state: EvmState) => { state.pop.map(_._2) }

    def mload   = (o: EvmMemory.Offset) =>                 (state: EvmState) => { state.memLoad(o).flatMap { case (w, s) => s.push(w) } }
    def mstore  = (o: EvmMemory.Offset) => (w: EvmWord) => (state: EvmState) => { state.memStore(o, w) }
    def mstore8 = (o: EvmMemory.Offset) => (w: EvmWord) => (state: EvmState) => { state.memStore(o, (w.bytes(0))) }

    def sload   = (k: EvmStorage.Key) => (state: EvmState) => { state.sget(k).flatMap(v => state.push(v)) }
    def sstore  = (k: EvmStorage.Key) => (v: EvmStorage.Value) => (state: EvmState) => { state.sput(k, v) }

    def pc      = (state: EvmState) => { state.push(state.execution.pc) }
    def msize   = (state: EvmState) => { state.push(state.memSize) }
    def push(bytes: Int) = (state: EvmState) => { }
  }

}

