package com.github.sethereum.protocol

import scodec.codecs._
import com.github.sethereum.rlp._


sealed abstract class PacketType(val id: Int)
object PacketType {

  case object Ping          extends PacketType(1)
  case object Pong          extends PacketType(2)
  case object FindNeighbors extends PacketType(3)
  case object Neighbors     extends PacketType(4)

  val map = Seq(Ping, Pong, FindNeighbors, Neighbors).map(v => (v -> v.id)).toMap
}

sealed abstract class Packet {
  val hash: String
}
object Packet {
}
case class Ping(hash: String) extends Packet
case class Pong(hash: String) extends Packet
case class FindNeighbors(hash: String) extends Packet
case class Neighbors(hash: String) extends Packet


object Packet {

  val baseCodec = ("hash" | bits(512))

  val codec = (
    ("packet type" | mappedEnum(uint8, PacketType.map))
  )

  val codec2 = discriminated[Packet].by(uint8)
    .\ (1) { case p: Ping => p } ((("hash" | bits(512))).as[Ping])

}
