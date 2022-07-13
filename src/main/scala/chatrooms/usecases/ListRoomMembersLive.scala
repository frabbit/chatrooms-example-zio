package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.RoomName
import chatrooms.domain.ServerMessage
import zio.stream.ZStream
import chatrooms.domain.Client
import chatrooms.domain.ServerError

final case class ListRoomMembersLive(stateRef:TRef[ServerState], server:SocketServer) extends ListRoomMembers:

  def map (s:ServerState, name:RoomName, clientId:ClientId):ServerMessage =
    s.getRoomMemberNames(name).match {
      case Some(members) => ServerMessage.AllRoomMembers(name, members)
      case None => ServerMessage.Error(ServerError.RoomNotFound(name))
      //case Left(ServerState.ClientExists) => (ServerMessage.Error(ServerError.AlreadyJoined), s)
    }
  def run(clientId:ClientId, name: RoomName): ZIO[Any, Nothing, ServerMessage] =
    stateRef.get.commit.map(map(_, name, clientId))


object ListRoomMembersLive:
  val layer:ZLayer[TRef[ServerState] & SocketServer, Nothing, ListRoomMembers] = ZLayer.fromFunction(ListRoomMembersLive.apply)

