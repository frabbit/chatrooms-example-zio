package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage

trait SendDirectMessage {
  def sendDirectMessage(from:ClientId, to:UserName, msg:String):ZIO[Any, Nothing, Unit]
}

object SendDirectMessage {
  def sendDirectMessage(from:ClientId, to:UserName, msg:String):ZIO[SendDirectMessage, Nothing, Unit] =
    ZIO.serviceWith[SendDirectMessage](_.sendDirectMessage(from, to, msg))
}
