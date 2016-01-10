package com.github.sethereum.evm

import com.github.sethereum.evm.EvmStateTransition._
import com.github.sethereum.evm.EvmWord.ImplicitConversions._
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.KeccakDigest
import scodec.Attempt.Successful
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult}

import scala.annotation.switch
import scala.util.{Failure, Try}


/**
 * EVM operation represented as a state transition function.
 *
 * @param opcode
 * @param transition
 */
sealed abstract class EvmOp(val opcode: Int)(val pop: Int, push: Int)(val transition: StateTransitionFunction) {
  require(opcode >= 0 && opcode <= 0xff, f"invalid opcode 0x$opcode%2x")

  lazy val name = getClass.getSimpleName.filterNot(_ == '$')

  def size: Int = 1
  def code: Seq[Byte] = Seq(opcode.toByte)
  def apply(state: EvmState) = transition(state)
}

/**
 * EVM PUSH operations encapsulate data to be pushed.
 *
 * @param opcode
 * @param bytes
 */
sealed abstract class EvmPushOp(opcode: Int)(val bytes: Seq[Byte])
  extends EvmOp(opcode)(0, 1)(EvmOp.StateTransitionFunctions.push(bytes))
{
  require(opcode >= 0x60 && opcode <= 0x60 + EvmWord.BYTES, f"invalid push opcode 0x$opcode%2x")
  require(bytes.size == size - 1, s"push bytes size mismatch (required: ${size - 1}, actual: ${bytes.size})")

  override def size: Int = opcode - 0x60 + 2

  override def code: Seq[Byte] = super.code ++ bytes
}


object EvmOp {
  import StateTransitionFunctions._


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
  case object BYTE          extends EvmOp   (0x1a)  (2, 1)    (byte        )
  case object SHA3          extends EvmOp   (0x20)  (2, 1)    (sha3        )
  case object ADDRESS       extends EvmOp   (0x30)  (0, 1)    (address     )
  //  case object BALANCE       extends EvmOp   (0x31)  (1, 1)    (balance     )
  case object ORIGIN        extends EvmOp   (0x32)  (0, 1)    (origin      )
  case object CALLER        extends EvmOp   (0x33)  (0, 1)    (caller      )
  case object CALLVALUE     extends EvmOp   (0x34)  (0, 1)    (callvalue   )
  case object CALLDATALOAD  extends EvmOp   (0x35)  (1, 1)    (calldataload)
  case object CALLDATASIZE  extends EvmOp   (0x36)  (0, 1)    (calldatasize)
  case object CALLDATACOPY  extends EvmOp   (0x37)  (3, 0)    (calldatacopy)
  case object CODESIZE      extends EvmOp   (0x38)  (0, 1)    (codesize    )
  case object CODECOPY      extends EvmOp   (0x39)  (3, 0)    (codecopy    )
  //  case object GASPRICE      extends EvmOp   (0x3a)  (0, 1)    (gasprice    )
  //  case object EXTCODESIZE   extends EvmOp   (0x3b)  (1, 1)    (extcodesize )
  //  case object EXTCODECOPY   extends EvmOp   (0x3c)  (4, 0)    (extcodecopy )
  //  case object BLOCKHASH     extends EvmOp   (0x40)  (1, 1)    (blockhash   )
  case object COINBASE      extends EvmOp   (0x41)  (0, 1)    (coinbase    )
  case object TIMESTAMP     extends EvmOp   (0x42)  (0, 1)    (timestamp   )
  case object NUMBER        extends EvmOp   (0x43)  (0, 1)    (number      )
  case object DIFFICULTY    extends EvmOp   (0x44)  (0, 1)    (difficulty  )
  case object GASLIMIT      extends EvmOp   (0x45)  (0, 1)    (gaslimit    )
  case object POP           extends EvmOp   (0x50)  (1, 0)    (pop         )
  case object MLOAD         extends EvmOp   (0x51)  (1, 1)    (mload       )
  case object MSTORE        extends EvmOp   (0x52)  (2, 0)    (mstore      )
  case object MSTORE8       extends EvmOp   (0x53)  (2, 0)    (mstore8     )
  case object SLOAD         extends EvmOp   (0x54)  (1, 1)    (sload       )
  case object SSTORE        extends EvmOp   (0x55)  (2, 0)    (sstore      )
  case object JUMP          extends EvmOp   (0x56)  (1, 0)    (jump        )
  case object JUMPI         extends EvmOp   (0x57)  (2, 0)    (jumpi       )
  case object PC            extends EvmOp   (0x58)  (0, 1)    (pc          )
  case object MSIZE         extends EvmOp   (0x59)  (0, 1)    (msize       )
  //  case object GAS           extends EvmOp   (0x5a)  (0, 1)    (gas         )
  case object JUMPDEST      extends EvmOp   (0x5b)  (0, 0)    (jumpdest    )
  case class  PUSH1 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x60)  (bytes)
  case class  PUSH2 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x61)  (bytes)
  case class  PUSH3 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x62)  (bytes)
  case class  PUSH4 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x63)  (bytes)
  case class  PUSH5 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x64)  (bytes)
  case class  PUSH6 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x65)  (bytes)
  case class  PUSH7 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x66)  (bytes)
  case class  PUSH8 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x67)  (bytes)
  case class  PUSH9 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x68)  (bytes)
  case class  PUSH10(override val bytes: Seq[Byte])   extends EvmPushOp   (0x69)  (bytes)
  case class  PUSH11(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6a)  (bytes)
  case class  PUSH12(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6b)  (bytes)
  case class  PUSH13(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6c)  (bytes)
  case class  PUSH14(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6d)  (bytes)
  case class  PUSH15(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6e)  (bytes)
  case class  PUSH16(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6f)  (bytes)
  case class  PUSH17(override val bytes: Seq[Byte])   extends EvmPushOp   (0x70)  (bytes)
  case class  PUSH18(override val bytes: Seq[Byte])   extends EvmPushOp   (0x71)  (bytes)
  case class  PUSH19(override val bytes: Seq[Byte])   extends EvmPushOp   (0x72)  (bytes)
  case class  PUSH20(override val bytes: Seq[Byte])   extends EvmPushOp   (0x73)  (bytes)
  case class  PUSH21(override val bytes: Seq[Byte])   extends EvmPushOp   (0x74)  (bytes)
  case class  PUSH22(override val bytes: Seq[Byte])   extends EvmPushOp   (0x75)  (bytes)
  case class  PUSH23(override val bytes: Seq[Byte])   extends EvmPushOp   (0x76)  (bytes)
  case class  PUSH24(override val bytes: Seq[Byte])   extends EvmPushOp   (0x77)  (bytes)
  case class  PUSH25(override val bytes: Seq[Byte])   extends EvmPushOp   (0x78)  (bytes)
  case class  PUSH26(override val bytes: Seq[Byte])   extends EvmPushOp   (0x79)  (bytes)
  case class  PUSH27(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7a)  (bytes)
  case class  PUSH28(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7b)  (bytes)
  case class  PUSH29(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7c)  (bytes)
  case class  PUSH30(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7d)  (bytes)
  case class  PUSH31(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7e)  (bytes)
  case class  PUSH32(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7f)  (bytes)
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
  case object SWAP1         extends EvmOp       (0x90)  (2, 2)    (swap(1 )      )
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
  //  case object CREATE        extends EvmOp   (0xf0)  (3, 1)    (create      )
  //  case object CALL          extends EvmOp   (0xf1)  (7, 1)    (call        )
  //  case object CALLCODE      extends EvmOp   (0xf2)  (7, 1)    (callcode    )
  //  case object RETURN        extends EvmOp   (0xf3)  (2, 0)    (ret         )
  //  case object SUICIDE       extends EvmOp   (0xff)  (1, 0)    (suicide     )


  /**
   * EVM operation state transition functions.
   */
  object StateTransitionFunctions {
    import EvmState._

    val ZERO = BigInt(0)
    val ONE = BigInt(1)
    val MOD256 = ONE << 256

    def stop = (state: EvmState) => { Try(state) }
    def add    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x + y) }
    def mul    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x * y) }
    def sub    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x - y) }
    def div    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(if (y == ZERO) ZERO else x / y) }

    def iszero = (x: EvmWord) =>                (state: EvmState) => { state.push(if (x.zero) ONE else ZERO) }
    def and    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x & y) }
    def or     = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x | y) }
    def xor    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x ^ y) }
    def not    = (x: BigInt) =>                 (state: EvmState) => { state.push(~x) }
    def byte   = (i: Int)    => (w: EvmWord) => (state: EvmState) => { state.push(if (i < EvmWord.BYTES) EvmWord(w.padLeft.bytes(i)) else EvmWord.ZERO) }

    def sha3   = (begin: Int) => (len: Int) => (state: EvmState) => { state.memSlice(begin, begin + len).flatMap { case (m, s) => s.push(EvmWord(keccakDigest(m))) } }

    def address       = (state: EvmState) => { state.push(state.environment.address) }
    def origin        = (state: EvmState) => { state.push(state.environment.origin) }
    def caller        = (state: EvmState) => { state.push(state.environment.call.caller) }
    def callvalue     = (state: EvmState) => { state.push(state.environment.call.callValue) }
    def calldataload  = (offset: Int) => (state: EvmState) => { state.push(state.environment.call.callData.slice(offset, offset + EvmWord.BYTES).padTo(EvmWord.BYTES, 0.toByte)) }
    def calldatasize  = (state: EvmState) => { state.push(state.environment.call.callData.size) }
    def calldatacopy  = (to: Int) => (from: Int) => (len: Int) => (state: EvmState) => { state.memStore(to, state.environment.call.callData.slice(from, from + len).padTo(len, 0.toByte)) }
    def codesize      = (state: EvmState) => { state.push(state.execution.program.code.size) }
    def codecopy      = (to: Int) => (from: Int) => (len: Int) => (state: EvmState) => { state.memStore(to, state.execution.program.code.slice(from, from + len).padTo(len, STOP.opcode.toByte)) }

    def coinbase      = (state: EvmState) => { state.push(state.environment.block.header.beneficiary) }
    def timestamp     = (state: EvmState) => { state.push(state.environment.block.header.timestamp) }
    def number        = (state: EvmState) => { state.push(state.environment.block.header.number) }
    def difficulty    = (state: EvmState) => { state.push(state.environment.block.header.difficulty) }
    def gaslimit      = (state: EvmState) => { state.push(state.environment.block.header.gasLimit) }

    def pop     = (state: EvmState) => { state.pop.map(_._2) }

    def mload   = (offset: Int) =>                 (state: EvmState) => { state.memLoad(offset).flatMap { case (w, s) => s.push(w) } }
    def mstore  = (offset: Int) => (w: EvmWord) => (state: EvmState) => { state.memStore(offset, w) }
    def mstore8 = (offset: Int) => (w: EvmWord) => (state: EvmState) => { state.memStore(offset, (w.bytes(0))) }

    def sload   = (k: EvmStorage.Key) =>                          (state: EvmState) => { state.sget(k).flatMap(v => state.push(v)) }
    def sstore  = (k: EvmStorage.Key) => (v: EvmStorage.Value) => (state: EvmState) => { state.sput(k, v) }

    def jump             = (pc: Int) =>                    (state: EvmState) => { state.jump(pc) }
    def jumpi            = (pc: Int) => (cond: EvmWord) => (state: EvmState) => { if (cond.zero) Try(state) else state.jump(pc) }
    def pc               = (state: EvmState) => { state.push(state.execution.pc) }
    def msize            = (state: EvmState) => { state.push(state.memSize) }
    def jumpdest         = (state: EvmState) => { Try(state) }
    def push(bytes: Seq[Byte]) = (state: EvmState) => { state.push(EvmWord(bytes)) }
    def dup(i: Int)      = (state: EvmState) => { state.push(state.stack(i - 1)) }
    def swap(i: Int)     = (state: EvmState) => { Try(stackLens.set(state)((state.stack(i) +: state.stack.tail.take(i-1)) ++ (state.stack.head +: state.stack.drop(i+1)))) }
  }


  // Decode EVM operation
  // Except for PUSH operations, this simply means reading a single byte and mapping to the appropriate operation
  // Remaining bytes are returned to support repetitive calls, consuming a single variable-sized operation each time
  // Note the use of the @switch annotation to emit tablelookup
  // See: http://www.scala-lang.org/api/current/index.html#scala.annotation.switch
  def decode(code: Seq[Byte]): Try[(EvmOp, Seq[Byte])] = {
    Try {
      (code.head: @switch) match {
        case 0x00 => (STOP        , code.tail)
        case 0x01 => (ADD         , code.tail)
        case 0x02 => (MUL         , code.tail)
        case 0x03 => (SUB         , code.tail)
        case 0x04 => (DIV         , code.tail)
//        case 0x05 => (SDIV        , code.tail)
//        case 0x06 => (MOD         , code.tail)
//        case 0x07 => (SMOD        , code.tail)
//        case 0x08 => (ADDMOD      , code.tail)
//        case 0x09 => (MULMOD      , code.tail)
//        case 0x0a => (EXP         , code.tail)
//        case 0x0b => (SIGNEXTEND  , code.tail)
//        case 0x10 => (LT          , code.tail)
//        case 0x11 => (GT          , code.tail)
//        case 0x12 => (SLT         , code.tail)
//        case 0x13 => (SGT         , code.tail)
//        case 0x14 => (EQ          , code.tail)
        case 0x15 => (ISZERO      , code.tail)
        case 0x16 => (AND         , code.tail)
        case 0x17 => (OR          , code.tail)
        case 0x18 => (XOR         , code.tail)
        case 0x19 => (NOT         , code.tail)
        case 0x1a => (BYTE        , code.tail)
        case 0x20 => (SHA3        , code.tail)
        case 0x30 => (ADDRESS     , code.tail)
//        case 0x31 => (BALANCE     , code.tail)
        case 0x32 => (ORIGIN      , code.tail)
        case 0x33 => (CALLER      , code.tail)
        case 0x34 => (CALLVALUE   , code.tail)
        case 0x35 => (CALLDATALOAD, code.tail)
        case 0x36 => (CALLDATASIZE, code.tail)
        case 0x37 => (CALLDATACOPY, code.tail)
        case 0x38 => (CODESIZE    , code.tail)
        case 0x39 => (CODECOPY    , code.tail)
//        case 0x3a => (GASPRICE    , code.tail)
//        case 0x3b => (EXTCODESIZE , code.tail)
//        case 0x3c => (EXTCODECOPY , code.tail)
//        case 0x40 => (BLOCKHASH   , code.tail)
        case 0x41 => (COINBASE    , code.tail)
        case 0x42 => (TIMESTAMP   , code.tail)
        case 0x43 => (NUMBER      , code.tail)
        case 0x44 => (DIFFICULTY  , code.tail)
        case 0x45 => (GASLIMIT    , code.tail)
        case 0x50 => (POP         , code.tail)
        case 0x51 => (MLOAD       , code.tail)
        case 0x52 => (MSTORE      , code.tail)
        case 0x53 => (MSTORE8     , code.tail)
        case 0x54 => (SLOAD       , code.tail)
        case 0x55 => (SSTORE      , code.tail)
//        case 0x56 => (JUMP        , code.tail)
//        case 0x57 => (JUMPI       , code.tail)
        case 0x58 => (PC          , code.tail)
        case 0x59 => (MSIZE       , code.tail)
//        case 0x5a => (GAS         , code.tail)
        case 0x5b => (JUMPDEST    , code.tail)
        case 0x60 => code.splitAt(1 ) match { case (bytes, tail) => (PUSH1 (bytes), tail) }
        case 0x61 => code.splitAt(2 ) match { case (bytes, tail) => (PUSH2 (bytes), tail) }
        case 0x62 => code.splitAt(3 ) match { case (bytes, tail) => (PUSH3 (bytes), tail) }
        case 0x63 => code.splitAt(4 ) match { case (bytes, tail) => (PUSH4 (bytes), tail) }
        case 0x64 => code.splitAt(5 ) match { case (bytes, tail) => (PUSH5 (bytes), tail) }
        case 0x65 => code.splitAt(6 ) match { case (bytes, tail) => (PUSH6 (bytes), tail) }
        case 0x66 => code.splitAt(7 ) match { case (bytes, tail) => (PUSH7 (bytes), tail) }
        case 0x67 => code.splitAt(8 ) match { case (bytes, tail) => (PUSH8 (bytes), tail) }
        case 0x68 => code.splitAt(9 ) match { case (bytes, tail) => (PUSH9 (bytes), tail) }
        case 0x69 => code.splitAt(10) match { case (bytes, tail) => (PUSH10(bytes), tail) }
        case 0x6a => code.splitAt(11) match { case (bytes, tail) => (PUSH11(bytes), tail) }
        case 0x6b => code.splitAt(12) match { case (bytes, tail) => (PUSH12(bytes), tail) }
        case 0x6c => code.splitAt(13) match { case (bytes, tail) => (PUSH13(bytes), tail) }
        case 0x6d => code.splitAt(14) match { case (bytes, tail) => (PUSH14(bytes), tail) }
        case 0x6e => code.splitAt(15) match { case (bytes, tail) => (PUSH15(bytes), tail) }
        case 0x6f => code.splitAt(16) match { case (bytes, tail) => (PUSH16(bytes), tail) }
        case 0x70 => code.splitAt(17) match { case (bytes, tail) => (PUSH17(bytes), tail) }
        case 0x71 => code.splitAt(18) match { case (bytes, tail) => (PUSH18(bytes), tail) }
        case 0x72 => code.splitAt(19) match { case (bytes, tail) => (PUSH19(bytes), tail) }
        case 0x73 => code.splitAt(20) match { case (bytes, tail) => (PUSH20(bytes), tail) }
        case 0x74 => code.splitAt(21) match { case (bytes, tail) => (PUSH21(bytes), tail) }
        case 0x75 => code.splitAt(22) match { case (bytes, tail) => (PUSH22(bytes), tail) }
        case 0x76 => code.splitAt(23) match { case (bytes, tail) => (PUSH23(bytes), tail) }
        case 0x77 => code.splitAt(24) match { case (bytes, tail) => (PUSH24(bytes), tail) }
        case 0x78 => code.splitAt(25) match { case (bytes, tail) => (PUSH25(bytes), tail) }
        case 0x79 => code.splitAt(26) match { case (bytes, tail) => (PUSH26(bytes), tail) }
        case 0x7a => code.splitAt(27) match { case (bytes, tail) => (PUSH27(bytes), tail) }
        case 0x7b => code.splitAt(28) match { case (bytes, tail) => (PUSH28(bytes), tail) }
        case 0x7c => code.splitAt(29) match { case (bytes, tail) => (PUSH29(bytes), tail) }
        case 0x7d => code.splitAt(30) match { case (bytes, tail) => (PUSH30(bytes), tail) }
        case 0x7e => code.splitAt(31) match { case (bytes, tail) => (PUSH31(bytes), tail) }
        case 0x7f => code.splitAt(32) match { case (bytes, tail) => (PUSH32(bytes), tail) }
        case 0x80 => (DUP1        , code.tail)
        case 0x81 => (DUP2        , code.tail)
        case 0x82 => (DUP3        , code.tail)
        case 0x83 => (DUP4        , code.tail)
        case 0x84 => (DUP5        , code.tail)
        case 0x85 => (DUP6        , code.tail)
        case 0x86 => (DUP7        , code.tail)
        case 0x87 => (DUP8        , code.tail)
        case 0x88 => (DUP9        , code.tail)
        case 0x89 => (DUP10       , code.tail)
        case 0x8a => (DUP11       , code.tail)
        case 0x8b => (DUP12       , code.tail)
        case 0x8c => (DUP13       , code.tail)
        case 0x8d => (DUP14       , code.tail)
        case 0x8e => (DUP15       , code.tail)
        case 0x8f => (DUP16       , code.tail)
        case 0x90 => (SWAP1       , code.tail)
        case 0x91 => (SWAP2       , code.tail)
        case 0x92 => (SWAP3       , code.tail)
        case 0x93 => (SWAP4       , code.tail)
        case 0x94 => (SWAP5       , code.tail)
        case 0x95 => (SWAP6       , code.tail)
        case 0x96 => (SWAP7       , code.tail)
        case 0x97 => (SWAP8       , code.tail)
        case 0x98 => (SWAP9       , code.tail)
        case 0x99 => (SWAP10      , code.tail)
        case 0x9a => (SWAP11      , code.tail)
        case 0x9b => (SWAP12      , code.tail)
        case 0x9c => (SWAP13      , code.tail)
        case 0x9d => (SWAP14      , code.tail)
        case 0x9e => (SWAP15      , code.tail)
        case 0x9f => (SWAP16      , code.tail)
//        case 0xa0 => (LOG0        , code.tail)
//        case 0xa1 => (LOG1        , code.tail)
//        case 0xa2 => (LOG2        , code.tail)
//        case 0xa3 => (LOG3        , code.tail)
//        case 0xa4 => (LOG4        , code.tail)
//        case 0xf0 => (CREATE      , code.tail)
//        case 0xf1 => (CALL        , code.tail)
//        case 0xf2 => (CALLCODE    , code.tail)
//        case 0xf3 => (RETURN      , code.tail)
//        case 0xff => (SUICIDE     , code.tail)

        case opcode => throw new EvmDecodeException(f"invalid opcode 0x$opcode%2x")
      }
    } recoverWith {
      case e: IllegalArgumentException => Failure(new EvmDecodeException(e.getMessage))
    }
  }

}


class EvmDecodeException(msg: String) extends EvmException(msg)