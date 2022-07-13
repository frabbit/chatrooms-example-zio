package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage
import zio.stream.ZStream
import chatrooms.domain.Client
import chatrooms.domain.ServerError
import chatrooms.all.allSyntaxChatrooms

final case class ListRoomsLive(stateRef:TRef[ServerState]) extends ListRooms:

  def run(clientId:ClientId): ZIO[Any, Nothing, ServerMessage] =
    for {
      state <- stateRef.get.commit
      rooms = state.getAllRoomNames
    } yield ServerMessage.AllRoomNames(rooms)



object ListRoomsLive:
  val layer:ZLayer[TRef[ServerState], Nothing, ListRooms] = ZLayer.fromFunction(ListRoomsLive.apply)

