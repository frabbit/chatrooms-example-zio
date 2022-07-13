package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.RoomName
import chatrooms.domain.ServerMessage


trait ListRoomMembers {
  def run(clientId:ClientId, name:RoomName):ZIO[Any, Nothing, ServerMessage]
}

object ListRoomMembers {
  def run(clientId:ClientId, name:RoomName):ZIO[ListRoomMembers, Nothing, ServerMessage] =
    ZIO.serviceWithZIO[ListRoomMembers](_.run(clientId, name))
}

