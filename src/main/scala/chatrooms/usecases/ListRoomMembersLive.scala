package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.RoomName
import chatrooms.domain.ServerMessage
import zio.stream.ZStream
import chatrooms.domain.Client
import chatrooms.domain.ServerError

final case class ListRoomMembersLive(stateRef:TRef[ServerState]) extends ListRoomMembers:

  def map (s:ServerState, name:RoomName, clientId:ClientId):ServerMessage =
    (s.clientExists(clientId), s.isMember(clientId, name), s.getRoomMemberNames(name)).match {
      case (false, _, _) => ServerMessage.Error(ServerError.NotJoined)
      case (_, _, None) => ServerMessage.Error(ServerError.RoomNotFound(name))
      case (_, false, _) => ServerMessage.Error(ServerError.UserIsNotInRoom)
      case (_, _, Some(members)) => ServerMessage.AllRoomMembers(name, members)

    }
  def run(clientId:ClientId, name: RoomName): ZIO[Any, Nothing, ServerMessage] =
    for {
      result <- stateRef.get.commit.map(map(_, name, clientId))
    } yield result

object ListRoomMembersLive:
  val layer:ZLayer[TRef[ServerState], Nothing, ListRoomMembers] = ZLayer.fromFunction(ListRoomMembersLive.apply)
