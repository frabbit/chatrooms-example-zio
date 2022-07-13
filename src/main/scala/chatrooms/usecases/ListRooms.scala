package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage

trait ListRooms {
  def run(clientId:ClientId):ZIO[Any, Nothing, ServerMessage]
}

object ListRooms {
  def run(clientId:ClientId):ZIO[ListRooms, Nothing, ServerMessage] =
    ZIO.serviceWithZIO[ListRooms](_.run(clientId))
}

