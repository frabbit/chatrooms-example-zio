package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage
import zio.stream.ZStream
import chatrooms.domain.Client
import chatrooms.domain.ServerError

final case class JoinLive(stateRef:TRef[ServerState]) extends Join:
  def map (s:ServerState, name:UserName, clientId:ClientId):(ServerMessage, ServerState) =
    s.addClient(Client(clientId, name)).match {
      case Right(s1) => (ServerMessage.Acknowledge("join"), s1)
      case Left(ServerState.ClientExists) => (ServerMessage.Error(ServerError.AlreadyJoined), s)
      case Left(ServerState.UserNameTaken) => (ServerMessage.Error(ServerError.UserNameTaken), s)
    }
  def run(clientId:ClientId, name: UserName): ZIO[Any, Nothing, ServerMessage] =
    for {
      msg <- stateRef.modify(map(_, name, clientId)).commit
    } yield msg


object JoinLive:
  val layer:ZLayer[TRef[ServerState], Nothing, Join] = ZLayer.fromFunction(JoinLive.apply)

