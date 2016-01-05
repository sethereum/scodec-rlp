package com.github.sethereum.evm

import java.math.BigInteger

import scala.language.implicitConversions

class PositiveBigInt private (val self: BigInt) extends AnyVal {

  override def toString: String = self.toString()

  def toString(radix: Int): String = self.toString(radix)
}

object PositiveBigInt {

  def apply(x: BigInt): PositiveBigInt = {
    require(x > 0, s"non-positive value $x")
    new PositiveBigInt(x)
  }

  // Positive integer with a maximum size in bits
  def bits(n: Int): BigInt => PositiveBigInt = {
    val limit = BigInt(1) << n

    (x: BigInt) => {
      require(x < limit, s"value out of range (limit: $limit, value: $x)")
      apply(x)
    }
  }

  implicit def intToPositiveBigInt(x: Int): PositiveBigInt = PositiveBigInt(x)
  implicit def longToPositiveBigInt(x: Long): PositiveBigInt = PositiveBigInt(x)
  implicit def bigIntToPositiveBigInt(x: BigInt): PositiveBigInt = PositiveBigInt(x)
  implicit def javaBigIntegerToPositiveBigInt(x: BigInteger): PositiveBigInt = PositiveBigInt(x)

  implicit def positiveBigIntToBigInt(x: PositiveBigInt): BigInt = x.self
}
