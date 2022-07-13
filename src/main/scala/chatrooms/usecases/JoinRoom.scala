package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.RoomName
import chatrooms.domain.ServerMessage


trait JoinRoom {
  def run(clientId:ClientId, name:RoomName):ZIO[Any, Nothing, ServerMessage]
}

object JoinRoom {
  def run(clientId:ClientId, name:RoomName):ZIO[JoinRoom, Nothing, ServerMessage] =
    ZIO.serviceWithZIO[JoinRoom](_.run(clientId, name))
}

