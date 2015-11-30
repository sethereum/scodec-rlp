package com.github.sethereum.evm

sealed abstract class OpCode(id: Int, pop: Int, push: Int)

object OpCode {

  case object STOP          extends OpCode(0x00, 0, 0)
  case object ADD           extends OpCode(0x01, 2, 1)
  case object MUL           extends OpCode(0x02, 2, 1)
  case object SUB           extends OpCode(0x03, 2, 1)
  case object DIV           extends OpCode(0x04, 2, 1)
  case object SDIV          extends OpCode(0x05, 2, 1)
  case object MOD           extends OpCode(0x06, 3, 1)
  case object SMOD          extends OpCode(0x07, 3, 1)
  case object ADDMOD        extends OpCode(0x08, 3, 1)
  case object MULMOD        extends OpCode(0x09, 3, 1)
  case object EXP           extends OpCode(0x0a, 2, 1)
  case object SIGNEXTEND    extends OpCode(0x0b, 2, 1)
  case object LT            extends OpCode(0x10, 2, 1)
  case object GT            extends OpCode(0x11, 2, 1)
  case object SLT           extends OpCode(0x12, 2, 1)
  case object SGT           extends OpCode(0x13, 2, 1)
  case object EQ            extends OpCode(0x14, 2, 1)
  case object ISZERO        extends OpCode(0x15, 1, 1)
  case object AND           extends OpCode(0x16, 2, 1)
  case object OR            extends OpCode(0x17, 2, 1)
  case object XOR           extends OpCode(0x18, 2, 1)
  case object NOT           extends OpCode(0x19, 1, 1)
  case object BYTE          extends OpCode(0x1a, 2, 1)
  case object SHA3          extends OpCode(0x20, 2, 1)
  case object ADDRESS       extends OpCode(0x30, 0, 1)
  case object BALANCE       extends OpCode(0x31, 1, 1)
  case object ORIGIN        extends OpCode(0x32, 0, 1)
  case object CALLER        extends OpCode(0x33, 0, 1)
  case object CALLVALUE     extends OpCode(0x34, 0, 1)
  case object CALLDATALOAD  extends OpCode(0x35, 1, 1)
  case object CALLDATASIZE  extends OpCode(0x36, 0, 1)
  case object CALLDATACOPY  extends OpCode(0x37, 3, 0)
  case object CODESIZE      extends OpCode(0x38, 0, 1)
  case object CODECOPY      extends OpCode(0x39, 3, 0)
  case object GASPRICE      extends OpCode(0x3a, 0, 1)
  case object EXTCODESIZE   extends OpCode(0x3b, 1, 1)
  case object EXTCODECOPY   extends OpCode(0x3c, 4, 0)
  case object BLOCKHASH     extends OpCode(0x40, 1, 1)
  case object COINBASE      extends OpCode(0x41, 0, 1)
  case object TIMESTAMP     extends OpCode(0x42, 0, 1)
  case object NUMBER        extends OpCode(0x43, 0, 1)
  case object DIFFICULTY    extends OpCode(0x44, 0, 1)
  case object GASLIMIT      extends OpCode(0x45, 0, 1)
  case object POP           extends OpCode(0x50, 1, 0)
  case object MLOAD         extends OpCode(0x51, 1, 1)
  case object MSTORE        extends OpCode(0x52, 2, 0)
  case object MSTORE8       extends OpCode(0x53, 2, 0)
  case object SLOAD         extends OpCode(0x54, 1, 1)
  case object SSTORE        extends OpCode(0x55, 2, 0)
  case object JUMP          extends OpCode(0x56, 1, 0)
  case object JUMPI         extends OpCode(0x57, 2, 0)
  case object PC            extends OpCode(0x58, 0, 1)
  case object MSIZE         extends OpCode(0x59, 0, 1)
  case object GAS           extends OpCode(0x5a, 0, 1)
  case object JUMPDEST      extends OpCode(0x5b, 0, 0)
  case object PUSH1         extends OpCode(0x60, 0, 1)
  case object PUSH2         extends OpCode(0x61, 0, 1)
  case object PUSH3         extends OpCode(0x62, 0, 1)
  case object PUSH4         extends OpCode(0x63, 0, 1)
  case object PUSH5         extends OpCode(0x64, 0, 1)
  case object PUSH6         extends OpCode(0x65, 0, 1)
  case object PUSH7         extends OpCode(0x66, 0, 1)
  case object PUSH8         extends OpCode(0x67, 0, 1)
  case object PUSH9         extends OpCode(0x68, 0, 1)
  case object PUSH10        extends OpCode(0x69, 0, 1)
  case object PUSH11        extends OpCode(0x6a, 0, 1)
  case object PUSH12        extends OpCode(0x6b, 0, 1)
  case object PUSH13        extends OpCode(0x6c, 0, 1)
  case object PUSH14        extends OpCode(0x6d, 0, 1)
  case object PUSH15        extends OpCode(0x6e, 0, 1)
  case object PUSH16        extends OpCode(0x6f, 0, 1)
  case object PUSH17        extends OpCode(0x70, 0, 1)
  case object PUSH18        extends OpCode(0x71, 0, 1)
  case object PUSH19        extends OpCode(0x72, 0, 1)
  case object PUSH20        extends OpCode(0x73, 0, 1)
  case object PUSH21        extends OpCode(0x74, 0, 1)
  case object PUSH22        extends OpCode(0x75, 0, 1)
  case object PUSH23        extends OpCode(0x76, 0, 1)
  case object PUSH24        extends OpCode(0x77, 0, 1)
  case object PUSH25        extends OpCode(0x78, 0, 1)
  case object PUSH26        extends OpCode(0x79, 0, 1)
  case object PUSH27        extends OpCode(0x7a, 0, 1)
  case object PUSH28        extends OpCode(0x7b, 0, 1)
  case object PUSH29        extends OpCode(0x7c, 0, 1)
  case object PUSH30        extends OpCode(0x7d, 0, 1)
  case object PUSH31        extends OpCode(0x7e, 0, 1)
  case object PUSH32        extends OpCode(0x7f, 0, 1)
  case object DUP1          extends OpCode(0x80, 1, 2)
  case object DUP2          extends OpCode(0x81, 2, 3)
  case object DUP3          extends OpCode(0x82, 3, 4)
  case object DUP4          extends OpCode(0x83, 4, 5)
  case object DUP5          extends OpCode(0x84, 5, 6)
  case object DUP6          extends OpCode(0x85, 6, 7)
  case object DUP7          extends OpCode(0x86, 7, 8)
  case object DUP8          extends OpCode(0x87, 8, 9)
  case object DUP9          extends OpCode(0x88, 9, 10)
  case object DUP10         extends OpCode(0x89, 10, 11)
  case object DUP11         extends OpCode(0x8a, 11, 12)
  case object DUP12         extends OpCode(0x8b, 12, 13)
  case object DUP13         extends OpCode(0x8c, 13, 14)
  case object DUP14         extends OpCode(0x8d, 14, 15)
  case object DUP15         extends OpCode(0x8e, 15, 16)
  case object DUP16         extends OpCode(0x8f, 16, 17)
  case object SWAP1         extends OpCode(0x90, 2, 2)
  case object SWAP2         extends OpCode(0x91, 3, 3)
  case object SWAP3         extends OpCode(0x92, 4, 4)
  case object SWAP4         extends OpCode(0x93, 5, 5)
  case object SWAP5         extends OpCode(0x94, 6, 6)
  case object SWAP6         extends OpCode(0x95, 7, 7)
  case object SWAP7         extends OpCode(0x96, 8, 8)
  case object SWAP8         extends OpCode(0x97, 9, 9)
  case object SWAP9         extends OpCode(0x98, 10, 10)
  case object SWAP10        extends OpCode(0x99, 11, 11)
  case object SWAP11        extends OpCode(0x9a, 12, 12)
  case object SWAP12        extends OpCode(0x9b, 13, 13)
  case object SWAP13        extends OpCode(0x9c, 14, 14)
  case object SWAP14        extends OpCode(0x9d, 15, 15)
  case object SWAP15        extends OpCode(0x9e, 16, 16)
  case object SWAP16        extends OpCode(0x9f, 17, 17)
  case object LOG0          extends OpCode(0xa0, 2, 0)
  case object LOG1          extends OpCode(0xa1, 3, 0)
  case object LOG2          extends OpCode(0xa2, 4, 0)
  case object LOG3          extends OpCode(0xa3, 5, 0)
  case object LOG4          extends OpCode(0xa4, 6, 0)
}
