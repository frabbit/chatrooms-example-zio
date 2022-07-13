package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage

trait Join {
  def run(clientId:ClientId, name:UserName):ZIO[Any, Nothing, ServerMessage]
}

object Join {
  def run(clientId:ClientId, name:UserName):ZIO[Join, Nothing, ServerMessage] =
    ZIO.serviceWithZIO[Join](_.run(clientId, name))
}

