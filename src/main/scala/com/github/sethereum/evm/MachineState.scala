package com.github.sethereum.evm

import java.math.BigInteger


case class StackItem(bytes: Array[Byte]) {
  require(bytes.length == 32, s"invalid stack item (length=${bytes.length})")
}

object StackItemOps {

  implicit def stackItemToBytes(item: StackItem): Array[Byte] = item.bytes

  implicit def bigIntegerToStackItem(bigInteger: BigInteger): StackItem = StackItem(bigInteger.toByteArray)

  implicit class BigIntegerStackOps(item: StackItem) {
    def +(other: StackItem): StackItem = new BigInteger(item).add(new BigInteger(other))
  }
}


case class MachineState(stack: Seq[StackItem])
