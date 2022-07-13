package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.RoomName
import chatrooms.domain.ServerMessage


trait SendMessageToRoom {
  def run(clientId:ClientId, name:RoomName, msg:String):ZIO[Any, Nothing, Unit]
}

object SendMessageToRoom {
  def run(clientId:ClientId, name:RoomName, msg:String):ZIO[SendMessageToRoom, Nothing, Unit] =
    ZIO.serviceWithZIO[SendMessageToRoom](_.run(clientId, name, msg))
}

