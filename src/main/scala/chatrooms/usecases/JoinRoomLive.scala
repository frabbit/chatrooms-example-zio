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

final case class JoinRoomLive(stateRef:TRef[ServerState], server:SocketServer) extends JoinRoom:
  def map (s:ServerState, name:RoomName, clientId:ClientId):(ServerMessage, ServerState) =
    s.joinRoom(name, clientId).match {
      case s1 => (ServerMessage.Acknowledge("joinRoom"), s1)
      //case Left(ServerState.ClientExists) => (ServerMessage.Error(ServerError.AlreadyJoined), s)
    }
  def run(clientId:ClientId, name: RoomName): ZIO[Any, Nothing, ServerMessage] =
    stateRef.modify(map(_, name, clientId)).commit


object JoinRoomLive:
  val layer:ZLayer[TRef[ServerState] & SocketServer, Nothing, JoinRoom] = ZLayer.fromFunction(JoinRoomLive.apply)
