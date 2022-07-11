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

final case class JoinLive(stateRef:TRef[ServerState], server:SocketServer) extends Join:
  def applyJoin (s:ServerState, name:UserName, clientId:ClientId) =

    val has = s.clients.exists((_, c) => c.id == clientId || c.name == name)
    val s1 = if has then s else s.addClient(Client(clientId, name))
    var response = if has then ServerMessage.Error(ServerError.AlreadyJoined()) else ServerMessage.Acknowledge("join")
    println((s, s1).toString)
    (response, s1)

  def join(clientId:ClientId, name: UserName): ZIO[Any, Nothing, ServerMessage] =
    for {
      msg <- stateRef.modify(applyJoin(_, name, clientId)).commit
    } yield msg


object JoinLive:
  val layer:ZLayer[TRef[ServerState] & SocketServer, Nothing, Join] = ZLayer.fromFunction(JoinLive.apply)

