package com.github.sethereum.evm

import com.github.sethereum.evm.EvmBlock._
import com.github.sethereum.rlp._
import scodec.codecs._


case class EvmBlock(header: Header = Header())

object EvmBlock {

  case class Header(
    parentHash        : EvmHash       = EvmHash.Empty,
    ommersHash        : EvmHash       = EvmHash.Empty,
    beneficiary       : EvmAddress    = EvmAddress.Zero,
    stateRoot         : EvmHash       = EvmHash.Empty,
    transactionsRoot  : EvmHash       = EvmHash.Empty,
    receiptsRoot      : EvmHash       = EvmHash.Empty,
    logsBloom         : EvmBloom      = EvmBloom.Empty,
    difficulty        : EvmDifficulty = EvmDifficulty.Minimum,
    number            : EvmNumber     = EvmNumber.Zero,
    gasLimit          : EvmGas        = EvmGas.MinimumLimit,
    gasUsed           : EvmGas        = EvmGas.Zero,
    timestamp         : EvmTimestamp  = EvmTimestamp.now(),
    extraData         : B_32          = Seq.empty,
    mixHash           : EvmHash       = EvmHash.Empty,
    nonce             : EvmNonce      = EvmNonce.Zero
  )

}