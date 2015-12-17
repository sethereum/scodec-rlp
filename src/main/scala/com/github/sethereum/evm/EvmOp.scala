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
import scala.util.Try


/**
 * EVM operation represented as a state transition function.
 *
 * @param opcode
 * @param transition
 */
sealed abstract class EvmOp(val opcode: Int)(val pop: Int, push: Int)(val transition: StateTransitionFunction) {
  require(opcode >= 0 && opcode <= 0xff, f"invalid opcode 0x$opcode%2x")

  def size: Int = 1
  def code: Seq[Byte] = Seq(opcode.toByte)
  def apply(state: EvmState) = transition(state)
}

/**
 * EVM PUSH operations which encapsulate data to be pushed.
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
  case object JUMPDEST      extends EvmOp   (0x5b)  (0, 0)    (jumpdest    )
  case class PUSH1 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x60)  (bytes)
  case class PUSH2 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x61)  (bytes)
  case class PUSH3 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x62)  (bytes)
  case class PUSH4 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x63)  (bytes)
  case class PUSH5 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x64)  (bytes)
  case class PUSH6 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x65)  (bytes)
  case class PUSH7 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x66)  (bytes)
  case class PUSH8 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x67)  (bytes)
  case class PUSH9 (override val bytes: Seq[Byte])   extends EvmPushOp   (0x68)  (bytes)
  case class PUSH10(override val bytes: Seq[Byte])   extends EvmPushOp   (0x69)  (bytes)
  case class PUSH11(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6a)  (bytes)
  case class PUSH12(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6b)  (bytes)
  case class PUSH13(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6c)  (bytes)
  case class PUSH14(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6d)  (bytes)
  case class PUSH15(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6e)  (bytes)
  case class PUSH16(override val bytes: Seq[Byte])   extends EvmPushOp   (0x6f)  (bytes)
  case class PUSH17(override val bytes: Seq[Byte])   extends EvmPushOp   (0x70)  (bytes)
  case class PUSH18(override val bytes: Seq[Byte])   extends EvmPushOp   (0x71)  (bytes)
  case class PUSH19(override val bytes: Seq[Byte])   extends EvmPushOp   (0x72)  (bytes)
  case class PUSH20(override val bytes: Seq[Byte])   extends EvmPushOp   (0x73)  (bytes)
  case class PUSH21(override val bytes: Seq[Byte])   extends EvmPushOp   (0x74)  (bytes)
  case class PUSH22(override val bytes: Seq[Byte])   extends EvmPushOp   (0x75)  (bytes)
  case class PUSH23(override val bytes: Seq[Byte])   extends EvmPushOp   (0x76)  (bytes)
  case class PUSH24(override val bytes: Seq[Byte])   extends EvmPushOp   (0x77)  (bytes)
  case class PUSH25(override val bytes: Seq[Byte])   extends EvmPushOp   (0x78)  (bytes)
  case class PUSH26(override val bytes: Seq[Byte])   extends EvmPushOp   (0x79)  (bytes)
  case class PUSH27(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7a)  (bytes)
  case class PUSH28(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7b)  (bytes)
  case class PUSH29(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7c)  (bytes)
  case class PUSH30(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7d)  (bytes)
  case class PUSH31(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7e)  (bytes)
  case class PUSH32(override val bytes: Seq[Byte])   extends EvmPushOp   (0x7f)  (bytes)
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


  /**
   * EVM operation state transition functions.
   */
  object StateTransitionFunctions {
    import EvmState._

    val ZERO = BigInt(0)
    val ONE = BigInt(1)
    val MOD256 = ONE << 256

    def stop = (state: EvmState) => { state.stop }
    def add    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x + y) }
    def mul    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x * y) }
    def sub    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(x - y) }
    def div    = (x: BigInt) => (y: BigInt)  => (state: EvmState) => { state.push(if (y == ZERO) ZERO else x / y) }

    def iszero = (x: BigInt) =>                 (state: EvmState) => { state.push(if (x == ZERO) ONE else ZERO) }
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
    def codesize      = (state: EvmState) => { state.push(state.execution.code.size) }
    def codecopy      = (to: Int) => (from: Int) => (len: Int) => (state: EvmState) => { state.memStore(to, state.execution.code.slice(from, from + len).padTo(len, STOP.opcode.toByte)) }

    def pop     = (state: EvmState) => { state.pop.map(_._2) }

    def mload   = (offset: Int) =>                 (state: EvmState) => { state.memLoad(offset).flatMap { case (w, s) => s.push(w) } }
    def mstore  = (offset: Int) => (w: EvmWord) => (state: EvmState) => { state.memStore(offset, w) }
    def mstore8 = (offset: Int) => (w: EvmWord) => (state: EvmState) => { state.memStore(offset, (w.bytes(0))) }

    def sload   = (k: EvmStorage.Key) =>                          (state: EvmState) => { state.sget(k).flatMap(v => state.push(v)) }
    def sstore  = (k: EvmStorage.Key) => (v: EvmStorage.Value) => (state: EvmState) => { state.sput(k, v) }

    //    def jump             = (addr: Int) => (state: EvmState) => { state.push(state.execution.pc) }
    def pc               = (state: EvmState) => { state.push(state.execution.pc) }
    def msize            = (state: EvmState) => { state.push(state.memSize) }
    def jumpdest         = (state: EvmState) => { Try(state) }
    def push(bytes: Seq[Byte]) = (state: EvmState) => { state.push(EvmWord(bytes)) }
    def dup(i: Int)      = (state: EvmState) => { state.push(state.stack(i - 1)) }
    def swap1            = (state: EvmState) => { Try(stackLens.set(state)(state.stack(1) +: state.stack(0) +: state.stack.drop(2))) }
    def swap(i: Int)     = (state: EvmState) => { Try(stackLens.set(state)((state.stack(i) +: state.stack.tail.take(i-1)) ++ (state.stack.head +: state.stack.drop(i+1)))) }
  }

  def keccakDigest(bytes: Seq[Byte]): Seq[Byte] = {
    val digest: Digest = new KeccakDigest(256)
    val out = Array.ofDim[Byte](256/8)

    digest.update(bytes.toArray[Byte], 0, bytes.length)
    digest.doFinal(out, 0)
    out
  }


  // EVM operation codec (to/from bytes)
  val codec: Codec[EvmOp] = {

    def encode(op: EvmOp): Attempt[BitVector] = bytes.encode(ByteVector(op.code))

    // Decode EVM operation
    // Except for PUSH operations, this simply means reading a single byte and mapping to the appropriate operation
    // Note the use of the @switch annotation to emit tablelookup
    // See: http://www.scala-lang.org/api/current/index.html#scala.annotation.switch
    def decode(bits: BitVector): Attempt[DecodeResult[EvmOp]] = for {
      opcode <- scodec.codecs.byte.decode(bits)
      op <- (opcode.value: @switch) match {
        case 0x00 => Successful(DecodeResult(STOP        , opcode.remainder))
        case 0x01 => Successful(DecodeResult(ADD         , opcode.remainder))
        case 0x02 => Successful(DecodeResult(MUL         , opcode.remainder))
        case 0x03 => Successful(DecodeResult(SUB         , opcode.remainder))
        case 0x04 => Successful(DecodeResult(DIV         , opcode.remainder))
        //      case 0x05 => Successful(DecodeResult(SDIV        , opcode.remainder))
        //      case 0x06 => Successful(DecodeResult(MOD         , opcode.remainder))
        //      case 0x07 => Successful(DecodeResult(SMOD        , opcode.remainder))
        //      case 0x08 => Successful(DecodeResult(ADDMOD      , opcode.remainder))
        //      case 0x09 => Successful(DecodeResult(MULMOD      , opcode.remainder))
        //      case 0x0a => Successful(DecodeResult(EXP         , opcode.remainder))
        //      case 0x0b => Successful(DecodeResult(SIGNEXTEND  , opcode.remainder))
        //      case 0x10 => Successful(DecodeResult(LT          , opcode.remainder))
        //      case 0x11 => Successful(DecodeResult(GT          , opcode.remainder))
        //      case 0x12 => Successful(DecodeResult(SLT         , opcode.remainder))
        //      case 0x13 => Successful(DecodeResult(SGT         , opcode.remainder))
        //      case 0x14 => Successful(DecodeResult(EQ          , opcode.remainder))
        case 0x15 => Successful(DecodeResult(ISZERO      , opcode.remainder))
        case 0x16 => Successful(DecodeResult(AND         , opcode.remainder))
        case 0x17 => Successful(DecodeResult(OR          , opcode.remainder))
        case 0x18 => Successful(DecodeResult(XOR         , opcode.remainder))
        case 0x19 => Successful(DecodeResult(NOT         , opcode.remainder))
        case 0x1a => Successful(DecodeResult(BYTE        , opcode.remainder))
        case 0x20 => Successful(DecodeResult(SHA3        , opcode.remainder))
        case 0x30 => Successful(DecodeResult(ADDRESS     , opcode.remainder))
        //      case 0x31 => Successful(DecodeResult(BALANCE     , opcode.remainder))
        case 0x32 => Successful(DecodeResult(ORIGIN      , opcode.remainder))
        case 0x33 => Successful(DecodeResult(CALLER      , opcode.remainder))
        case 0x34 => Successful(DecodeResult(CALLVALUE   , opcode.remainder))
        case 0x35 => Successful(DecodeResult(CALLDATALOAD, opcode.remainder))
        case 0x36 => Successful(DecodeResult(CALLDATASIZE, opcode.remainder))
        case 0x37 => Successful(DecodeResult(CALLDATACOPY, opcode.remainder))
        case 0x38 => Successful(DecodeResult(CODESIZE    , opcode.remainder))
        case 0x39 => Successful(DecodeResult(CODECOPY    , opcode.remainder))
        //      case 0x3a => Successful(DecodeResult(GASPRICE    , opcode.remainder))
        //      case 0x3b => Successful(DecodeResult(EXTCODESIZE , opcode.remainder))
        //      case 0x3c => Successful(DecodeResult(EXTCODECOPY , opcode.remainder))
        //      case 0x40 => Successful(DecodeResult(BLOCKHASH   , opcode.remainder))
        //      case 0x41 => Successful(DecodeResult(COINBASE    , opcode.remainder))
        //      case 0x42 => Successful(DecodeResult(TIMESTAMP   , opcode.remainder))
        //      case 0x43 => Successful(DecodeResult(NUMBER      , opcode.remainder))
        //      case 0x44 => Successful(DecodeResult(DIFFICULTY  , opcode.remainder))
        //      case 0x45 => Successful(DecodeResult(GASLIMIT    , opcode.remainder))
        case 0x50 => Successful(DecodeResult(POP         , opcode.remainder))
        case 0x51 => Successful(DecodeResult(MLOAD       , opcode.remainder))
        case 0x52 => Successful(DecodeResult(MSTORE      , opcode.remainder))
        case 0x53 => Successful(DecodeResult(MSTORE8     , opcode.remainder))
        case 0x54 => Successful(DecodeResult(SLOAD       , opcode.remainder))
        case 0x55 => Successful(DecodeResult(SSTORE      , opcode.remainder))
        //      case 0x56 => Successful(DecodeResult(JUMP        , opcode.remainder))
        //      case 0x57 => Successful(DecodeResult(JUMPI       , opcode.remainder))
        case 0x58 => Successful(DecodeResult(PC          , opcode.remainder))
        case 0x59 => Successful(DecodeResult(MSIZE       , opcode.remainder))
        //      case 0x5a => Successful(DecodeResult(GAS         , opcode.remainder))
        case 0x5b => Successful(DecodeResult(JUMPDEST    , opcode.remainder))
        case 0x60 => bytes(1 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH1 (bytes.value.toSeq), bytes.remainder)))
        case 0x61 => bytes(2 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH2 (bytes.value.toSeq), bytes.remainder)))
        case 0x62 => bytes(3 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH3 (bytes.value.toSeq), bytes.remainder)))
        case 0x63 => bytes(4 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH4 (bytes.value.toSeq), bytes.remainder)))
        case 0x64 => bytes(5 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH5 (bytes.value.toSeq), bytes.remainder)))
        case 0x65 => bytes(6 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH6 (bytes.value.toSeq), bytes.remainder)))
        case 0x66 => bytes(7 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH7 (bytes.value.toSeq), bytes.remainder)))
        case 0x67 => bytes(8 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH8 (bytes.value.toSeq), bytes.remainder)))
        case 0x68 => bytes(9 ).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH9 (bytes.value.toSeq), bytes.remainder)))
        case 0x69 => bytes(10).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH10(bytes.value.toSeq), bytes.remainder)))
        case 0x6a => bytes(11).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH11(bytes.value.toSeq), bytes.remainder)))
        case 0x6b => bytes(12).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH12(bytes.value.toSeq), bytes.remainder)))
        case 0x6c => bytes(13).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH13(bytes.value.toSeq), bytes.remainder)))
        case 0x6d => bytes(14).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH14(bytes.value.toSeq), bytes.remainder)))
        case 0x6e => bytes(15).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH15(bytes.value.toSeq), bytes.remainder)))
        case 0x6f => bytes(16).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH16(bytes.value.toSeq), bytes.remainder)))
        case 0x70 => bytes(17).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH17(bytes.value.toSeq), bytes.remainder)))
        case 0x71 => bytes(18).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH18(bytes.value.toSeq), bytes.remainder)))
        case 0x72 => bytes(19).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH19(bytes.value.toSeq), bytes.remainder)))
        case 0x73 => bytes(20).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH20(bytes.value.toSeq), bytes.remainder)))
        case 0x74 => bytes(21).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH21(bytes.value.toSeq), bytes.remainder)))
        case 0x75 => bytes(22).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH22(bytes.value.toSeq), bytes.remainder)))
        case 0x76 => bytes(23).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH23(bytes.value.toSeq), bytes.remainder)))
        case 0x77 => bytes(24).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH24(bytes.value.toSeq), bytes.remainder)))
        case 0x78 => bytes(25).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH25(bytes.value.toSeq), bytes.remainder)))
        case 0x79 => bytes(26).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH26(bytes.value.toSeq), bytes.remainder)))
        case 0x7a => bytes(27).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH27(bytes.value.toSeq), bytes.remainder)))
        case 0x7b => bytes(28).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH28(bytes.value.toSeq), bytes.remainder)))
        case 0x7c => bytes(29).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH29(bytes.value.toSeq), bytes.remainder)))
        case 0x7d => bytes(30).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH30(bytes.value.toSeq), bytes.remainder)))
        case 0x7e => bytes(31).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH31(bytes.value.toSeq), bytes.remainder)))
        case 0x7f => bytes(32).decode(opcode.remainder).flatMap(bytes => Successful(DecodeResult(PUSH32(bytes.value.toSeq), bytes.remainder)))
        case 0x80 => Successful(DecodeResult(DUP1        , opcode.remainder))
        case 0x81 => Successful(DecodeResult(DUP2        , opcode.remainder))
        case 0x82 => Successful(DecodeResult(DUP3        , opcode.remainder))
        case 0x83 => Successful(DecodeResult(DUP4        , opcode.remainder))
        case 0x84 => Successful(DecodeResult(DUP5        , opcode.remainder))
        case 0x85 => Successful(DecodeResult(DUP6        , opcode.remainder))
        case 0x86 => Successful(DecodeResult(DUP7        , opcode.remainder))
        case 0x87 => Successful(DecodeResult(DUP8        , opcode.remainder))
        case 0x88 => Successful(DecodeResult(DUP9        , opcode.remainder))
        case 0x89 => Successful(DecodeResult(DUP10       , opcode.remainder))
        case 0x8a => Successful(DecodeResult(DUP11       , opcode.remainder))
        case 0x8b => Successful(DecodeResult(DUP12       , opcode.remainder))
        case 0x8c => Successful(DecodeResult(DUP13       , opcode.remainder))
        case 0x8d => Successful(DecodeResult(DUP14       , opcode.remainder))
        case 0x8e => Successful(DecodeResult(DUP15       , opcode.remainder))
        case 0x8f => Successful(DecodeResult(DUP16       , opcode.remainder))
        case 0x90 => Successful(DecodeResult(SWAP1       , opcode.remainder))
        case 0x91 => Successful(DecodeResult(SWAP2       , opcode.remainder))
        case 0x92 => Successful(DecodeResult(SWAP3       , opcode.remainder))
        case 0x93 => Successful(DecodeResult(SWAP4       , opcode.remainder))
        case 0x94 => Successful(DecodeResult(SWAP5       , opcode.remainder))
        case 0x95 => Successful(DecodeResult(SWAP6       , opcode.remainder))
        case 0x96 => Successful(DecodeResult(SWAP7       , opcode.remainder))
        case 0x97 => Successful(DecodeResult(SWAP8       , opcode.remainder))
        case 0x98 => Successful(DecodeResult(SWAP9       , opcode.remainder))
        case 0x99 => Successful(DecodeResult(SWAP10      , opcode.remainder))
        case 0x9a => Successful(DecodeResult(SWAP11      , opcode.remainder))
        case 0x9b => Successful(DecodeResult(SWAP12      , opcode.remainder))
        case 0x9c => Successful(DecodeResult(SWAP13      , opcode.remainder))
        case 0x9d => Successful(DecodeResult(SWAP14      , opcode.remainder))
        case 0x9e => Successful(DecodeResult(SWAP15      , opcode.remainder))
        case 0x9f => Successful(DecodeResult(SWAP16      , opcode.remainder))
        //      case 0xa0 => Successful(DecodeResult(LOG0        , opcode.remainder))
        //      case 0xa1 => Successful(DecodeResult(LOG1        , opcode.remainder))
        //      case 0xa2 => Successful(DecodeResult(LOG2        , opcode.remainder))
        //      case 0xa3 => Successful(DecodeResult(LOG3        , opcode.remainder))
        //      case 0xa4 => Successful(DecodeResult(LOG4        , opcode.remainder))
      }
    } yield op

    Codec(encode _, decode _)
  }

}
