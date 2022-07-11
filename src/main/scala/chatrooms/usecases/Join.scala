package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage

trait Join {
  def join(clientId:ClientId, name:UserName):ZIO[Any, Nothing, ServerMessage]
}

object Join {
  def join(clientId:ClientId, name:UserName):ZIO[Join, Nothing, ServerMessage] =
    ZIO.serviceWithZIO[Join](_.join(clientId, name))
}

