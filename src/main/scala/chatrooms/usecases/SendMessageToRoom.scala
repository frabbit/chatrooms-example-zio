package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.RoomName
import chatrooms.domain.ServerMessage


trait SendMessageToRoom {
  def run(clientId:ClientId, name:RoomName, msg:String):ZIO[Any, Nothing, Option[ServerMessage]]
}

object SendMessageToRoom {
  def run(clientId:ClientId, name:RoomName, msg:String):ZIO[SendMessageToRoom, Nothing, Option[ServerMessage]] =
    ZIO.serviceWithZIO[SendMessageToRoom](_.run(clientId, name, msg))
}
