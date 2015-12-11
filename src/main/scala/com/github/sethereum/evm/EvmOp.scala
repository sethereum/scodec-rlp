package com.github.sethereum.evm

import java.math.BigInteger

import com.github.sethereum.evm.EvmStateTransition._
import com.github.sethereum.evm.EvmWordConversions._
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.KeccakDigest

import scala.annotation.switch
import scala.util.Try


/**
 * EVM operation represented as a state transition function.
 *
 * @param opcode
 * @param transition
 */
sealed abstract class EvmOp(val opcode: Int)(val pop: Int, push: Int)(val transition: StateTransitionFunction) {
  def size: Int = 1
  def apply(state: EvmState) = transition(state)
}

sealed abstract class EvmPushOp(opcode: Int)(val bytes: Int)(transition: StateTransitionFunction)
  extends EvmOp(opcode)(0, 1)(transition)
{
  override def size: Int = bytes + 1
}


object EvmOp {
  import Function._

  // Efficiently map opcodes to operations
  // Note the use of the @switch annotation to emit tablelookup
  // See: http://www.scala-lang.org/api/current/index.html#scala.annotation.switch
  def unapply(opcode: Byte): Option[EvmOp] = (opcode: @switch) match {
    case 0x00 => Some(STOP        )
    case 0x01 => Some(ADD         )
    case 0x02 => Some(MUL         )
    case 0x03 => Some(SUB         )
    case 0x04 => Some(DIV         )
//    case 0x05 => Some(SDIV        )
//    case 0x06 => Some(MOD         )
//    case 0x07 => Some(SMOD        )
//    case 0x08 => Some(ADDMOD      )
//    case 0x09 => Some(MULMOD      )
//    case 0x0a => Some(EXP         )
//    case 0x0b => Some(SIGNEXTEND  )
//    case 0x10 => Some(LT          )
//    case 0x11 => Some(GT          )
//    case 0x12 => Some(SLT         )
//    case 0x13 => Some(SGT         )
//    case 0x14 => Some(EQ          )
    case 0x15 => Some(ISZERO      )
    case 0x16 => Some(AND         )
    case 0x17 => Some(OR          )
    case 0x18 => Some(XOR         )
    case 0x19 => Some(NOT         )
//    case 0x1a => Some(BYTE        )
//    case 0x20 => Some(SHA3        )
    case 0x30 => Some(ADDRESS     )
//    case 0x31 => Some(BALANCE     )
    case 0x32 => Some(ORIGIN      )
    case 0x33 => Some(CALLER      )
//    case 0x34 => Some(CALLVALUE   )
//    case 0x35 => Some(CALLDATALOAD)
//    case 0x36 => Some(CALLDATASIZE)
//    case 0x37 => Some(CALLDATACOPY)
//    case 0x38 => Some(CODESIZE    )
//    case 0x39 => Some(CODECOPY    )
//    case 0x3a => Some(GASPRICE    )
//    case 0x3b => Some(EXTCODESIZE )
//    case 0x3c => Some(EXTCODECOPY )
//    case 0x40 => Some(BLOCKHASH   )
//    case 0x41 => Some(COINBASE    )
//    case 0x42 => Some(TIMESTAMP   )
//    case 0x43 => Some(NUMBER      )
//    case 0x44 => Some(DIFFICULTY  )
//    case 0x45 => Some(GASLIMIT    )
    case 0x50 => Some(POP         )
    case 0x51 => Some(MLOAD       )
    case 0x52 => Some(MSTORE      )
    case 0x53 => Some(MSTORE8     )
    case 0x54 => Some(SLOAD       )
    case 0x55 => Some(SSTORE      )
//    case 0x56 => Some(JUMP        )
//    case 0x57 => Some(JUMPI       )
    case 0x58 => Some(PC          )
    case 0x59 => Some(MSIZE       )
//    case 0x5a => Some(GAS         )
//    case 0x5b => Some(JUMPDEST    )
    case 0x60 => Some(PUSH1       )
    case 0x61 => Some(PUSH2       )
    case 0x62 => Some(PUSH3       )
    case 0x63 => Some(PUSH4       )
    case 0x64 => Some(PUSH5       )
    case 0x65 => Some(PUSH6       )
    case 0x66 => Some(PUSH7       )
    case 0x67 => Some(PUSH8       )
    case 0x68 => Some(PUSH9       )
    case 0x69 => Some(PUSH10      )
    case 0x6a => Some(PUSH11      )
    case 0x6b => Some(PUSH12      )
    case 0x6c => Some(PUSH13      )
    case 0x6d => Some(PUSH14      )
    case 0x6e => Some(PUSH15      )
    case 0x6f => Some(PUSH16      )
    case 0x70 => Some(PUSH17      )
    case 0x71 => Some(PUSH18      )
    case 0x72 => Some(PUSH19      )
    case 0x73 => Some(PUSH20      )
    case 0x74 => Some(PUSH21      )
    case 0x75 => Some(PUSH22      )
    case 0x76 => Some(PUSH23      )
    case 0x77 => Some(PUSH24      )
    case 0x78 => Some(PUSH25      )
    case 0x79 => Some(PUSH26      )
    case 0x7a => Some(PUSH27      )
    case 0x7b => Some(PUSH28      )
    case 0x7c => Some(PUSH29      )
    case 0x7d => Some(PUSH30      )
    case 0x7e => Some(PUSH31      )
    case 0x7f => Some(PUSH32      )
    case 0x80 => Some(DUP1        )
    case 0x81 => Some(DUP2        )
    case 0x82 => Some(DUP3        )
    case 0x83 => Some(DUP4        )
    case 0x84 => Some(DUP5        )
    case 0x85 => Some(DUP6        )
    case 0x86 => Some(DUP7        )
    case 0x87 => Some(DUP8        )
    case 0x88 => Some(DUP9        )
    case 0x89 => Some(DUP10       )
    case 0x8a => Some(DUP11       )
    case 0x8b => Some(DUP12       )
    case 0x8c => Some(DUP13       )
    case 0x8d => Some(DUP14       )
    case 0x8e => Some(DUP15       )
    case 0x8f => Some(DUP16       )
    case 0x90 => Some(SWAP1       )
    case 0x91 => Some(SWAP2       )
    case 0x92 => Some(SWAP3       )
    case 0x93 => Some(SWAP4       )
    case 0x94 => Some(SWAP5       )
    case 0x95 => Some(SWAP6       )
    case 0x96 => Some(SWAP7       )
    case 0x97 => Some(SWAP8       )
    case 0x98 => Some(SWAP9       )
    case 0x99 => Some(SWAP10      )
    case 0x9a => Some(SWAP11      )
    case 0x9b => Some(SWAP12      )
    case 0x9c => Some(SWAP13      )
    case 0x9d => Some(SWAP14      )
    case 0x9e => Some(SWAP15      )
    case 0x9f => Some(SWAP16      )
//    case 0xa0 => Some(LOG0        )
//    case 0xa1 => Some(LOG1        )
//    case 0xa2 => Some(LOG2        )
//    case 0xa3 => Some(LOG3        )
//    case 0xa4 => Some(LOG4        )

    case _ => None
  }


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
  case object PUSH1         extends EvmPushOp   (0x60)  (1 )      (push(1)       )
  case object PUSH2         extends EvmPushOp   (0x61)  (2 )      (push(2 )      )
  case object PUSH3         extends EvmPushOp   (0x62)  (3 )      (push(3 )      )
  case object PUSH4         extends EvmPushOp   (0x63)  (4 )      (push(4 )      )
  case object PUSH5         extends EvmPushOp   (0x64)  (5 )      (push(5 )      )
  case object PUSH6         extends EvmPushOp   (0x65)  (6 )      (push(6 )      )
  case object PUSH7         extends EvmPushOp   (0x66)  (7 )      (push(7 )      )
  case object PUSH8         extends EvmPushOp   (0x67)  (8 )      (push(8 )      )
  case object PUSH9         extends EvmPushOp   (0x68)  (9 )      (push(9 )      )
  case object PUSH10        extends EvmPushOp   (0x69)  (10)      (push(10)      )
  case object PUSH11        extends EvmPushOp   (0x6a)  (11)      (push(11)      )
  case object PUSH12        extends EvmPushOp   (0x6b)  (12)      (push(12)      )
  case object PUSH13        extends EvmPushOp   (0x6c)  (13)      (push(13)      )
  case object PUSH14        extends EvmPushOp   (0x6d)  (14)      (push(14)      )
  case object PUSH15        extends EvmPushOp   (0x6e)  (15)      (push(15)      )
  case object PUSH16        extends EvmPushOp   (0x6f)  (16)      (push(16)      )
  case object PUSH17        extends EvmPushOp   (0x70)  (17)      (push(17)      )
  case object PUSH18        extends EvmPushOp   (0x71)  (18)      (push(18)      )
  case object PUSH19        extends EvmPushOp   (0x72)  (19)      (push(19)      )
  case object PUSH20        extends EvmPushOp   (0x73)  (20)      (push(20)      )
  case object PUSH21        extends EvmPushOp   (0x74)  (21)      (push(21)      )
  case object PUSH22        extends EvmPushOp   (0x75)  (22)      (push(22)      )
  case object PUSH23        extends EvmPushOp   (0x76)  (23)      (push(23)      )
  case object PUSH24        extends EvmPushOp   (0x77)  (24)      (push(24)      )
  case object PUSH25        extends EvmPushOp   (0x78)  (25)      (push(25)      )
  case object PUSH26        extends EvmPushOp   (0x79)  (26)      (push(26)      )
  case object PUSH27        extends EvmPushOp   (0x7a)  (27)      (push(27)      )
  case object PUSH28        extends EvmPushOp   (0x7b)  (28)      (push(28)      )
  case object PUSH29        extends EvmPushOp   (0x7c)  (29)      (push(29)      )
  case object PUSH30        extends EvmPushOp   (0x7d)  (30)      (push(30)      )
  case object PUSH31        extends EvmPushOp   (0x7e)  (31)      (push(31)      )
  case object PUSH32        extends EvmPushOp   (0x7f)  (32)      (push(32)      )
  case object DUP1          extends EvmOp       (0x80)  (1, 2)    (dup(1 )       )
  case object DUP2          extends EvmOp       (0x81)  (2, 3)    (dup(2 )       )
  case object DUP3          extends EvmOp       (0x82)  (3, 4)    (dup(3 )       )
  case object DUP4          extends EvmOp       (0x83)  (4, 5)    (dup(4 )       )
  case object DUP5          extends EvmOp       (0x84)  (5, 6)    (dup(5 )       )
  case object DUP6          extends EvmOp       (0x85)  (6, 7)    (dup(6 )       )
  case object DUP7          extends EvmOp       (0x86)  (7, 8)    (dup(7 )       )
  case object DUP8          extends EvmOp       (0x87)  (8, 9)    (dup(8 )       )
  case object DUP9          extends EvmOp       (0x88)  (9, 10)   (dup(9 )       )
  case object DUP10         extends EvmOp       (0x89)  (10, 11)  (dup(10)       )
  case object DUP11         extends EvmOp       (0x8a)  (11, 12)  (dup(11)       )
  case object DUP12         extends EvmOp       (0x8b)  (12, 13)  (dup(12)       )
  case object DUP13         extends EvmOp       (0x8c)  (13, 14)  (dup(13)       )
  case object DUP14         extends EvmOp       (0x8d)  (14, 15)  (dup(14)       )
  case object DUP15         extends EvmOp       (0x8e)  (15, 16)  (dup(15)       )
  case object DUP16         extends EvmOp       (0x8f)  (16, 17)  (dup(16)       )
  case object SWAP1         extends EvmOp       (0x90)  (2, 2)    (swap1         )
  case object SWAP2         extends EvmOp       (0x91)  (3, 3)    (swap(2 )      )
  case object SWAP3         extends EvmOp       (0x92)  (4, 4)    (swap(3 )      )
  case object SWAP4         extends EvmOp       (0x93)  (5, 5)    (swap(4 )      )
  case object SWAP5         extends EvmOp       (0x94)  (6, 6)    (swap(5 )      )
  case object SWAP6         extends EvmOp       (0x95)  (7, 7)    (swap(6 )      )
  case object SWAP7         extends EvmOp       (0x96)  (8, 8)    (swap(7 )      )
  case object SWAP8         extends EvmOp       (0x97)  (9, 9)    (swap(8 )      )
  case object SWAP9         extends EvmOp       (0x98)  (10, 10)  (swap(9 )      )
  case object SWAP10        extends EvmOp       (0x99)  (11, 11)  (swap(10)      )
  case object SWAP11        extends EvmOp       (0x9a)  (12, 12)  (swap(11)      )
  case object SWAP12        extends EvmOp       (0x9b)  (13, 13)  (swap(12)      )
  case object SWAP13        extends EvmOp       (0x9c)  (14, 14)  (swap(13)      )
  case object SWAP14        extends EvmOp       (0x9d)  (15, 15)  (swap(14)      )
  case object SWAP15        extends EvmOp       (0x9e)  (16, 16)  (swap(15)      )
  case object SWAP16        extends EvmOp       (0x9f)  (17, 17)  (swap(16)      )
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

    def sha3   = (begin: EvmMemory.Offset) => (end: EvmMemory.Offset) => (state: EvmState) => { state.memSlice(begin, end).flatMap { case (m, s) => s.push(EvmWord(keccakDigest(m))) } }

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
    def push(bytes: Int) = (state: EvmState) => { state.push(EvmWord(state.execution.code.slice(state.execution.pc + 1, state.execution.pc + bytes + 1))) }
    def dup(i: Int) = (state: EvmState) => { state.push(state.stack(i - 1)) }
    def swap1 = (state: EvmState) => { Try(EvmState.stackLens.set(state)(state.stack(1) +: state.stack(0) +: state.stack.tail.drop(1))) }
    def swap(i: Int) = (state: EvmState) => { Try(EvmState.stackLens.set(state)((state.stack(i) +: state.stack.tail.take(i-1)) ++ (state.stack.head +: state.stack.drop(i+1)))) }
  }

  def keccakDigest(bytes: Seq[Byte]): Seq[Byte] = {
    val digest: Digest = new KeccakDigest(256)
    val out = Array.ofDim[Byte](256/8)

    digest.update(bytes.toArray[Byte], 0, bytes.length)
    digest.doFinal(out, 0)
    out
  }

}

