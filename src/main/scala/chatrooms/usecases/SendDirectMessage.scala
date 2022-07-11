package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage

trait SendDirectMessage {
  def run(from:ClientId, to:UserName, msg:String):ZIO[Any, Nothing, ServerMessage]
}

object SendDirectMessage {
  def run(from:ClientId, to:UserName, msg:String):ZIO[SendDirectMessage, Nothing, ServerMessage] =
    ZIO.serviceWithZIO[SendDirectMessage](_.run(from, to, msg))
}
