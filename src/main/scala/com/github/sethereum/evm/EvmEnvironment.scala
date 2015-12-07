package com.github.sethereum.evm

case class EvmEnvironment(
  address: EvmAddress = EvmAddress.ZERO,
  origin: EvmAddress = EvmAddress.ZERO,
  caller: EvmAddress = EvmAddress.ZERO
)
