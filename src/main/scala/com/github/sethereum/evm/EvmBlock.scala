package com.github.sethereum.evm

import java.time.{LocalDateTime, ZoneOffset}


case class EvmBlock(header: EvmBlockHeader = EvmBlockHeader())

case class EvmBlockHeader(
  coinbase: EvmAddress = EvmAddress.ZERO,
  timestamp: EvmTimestamp = LocalDateTime.now.toEpochSecond(ZoneOffset.UTC),
  number: Long = 1,
  difficulty: Long = MinimumDifficulty,
  gasLimit: Long = MiniumGasLimit
  )

