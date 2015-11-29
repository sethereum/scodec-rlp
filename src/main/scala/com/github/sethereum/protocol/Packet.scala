package com.github.sethereum.protocol

import scodec.codecs._
import com.github.sethereum.rlp._


sealed trait PacketType
case object Ping extends PacketType
case object Pong extends PacketType
case object FindNeighbors extends PacketType
case object Neighbors extends PacketType

case class Packet(hash: String)

object Packet {

  val codec = ()

}
