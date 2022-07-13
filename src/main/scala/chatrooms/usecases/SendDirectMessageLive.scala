package chatrooms.usecases

import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage

final case class SendDirectMessageLive(stateRef:TRef[ServerState], server:SocketServer) extends SendDirectMessage:
  def run(from:ClientId, to: UserName, msg: String):ZIO[Any, Nothing, ServerMessage] =
    for {
        state <- stateRef.get.commit
        receiverClientId = state.clients.find(_._2.name == to).map(_._2.id)
        fromName = state.clients.find(_._2.id == from).map(_._2.name)
        //_ <- zio.Console.printLine("SendDirectMessage " ++ (receiverClientId, fromName).toString).ignore
        _ <- (fromName, receiverClientId).match {
          case (Some(f), Some(id)) => server.sendTo(id, ServerMessage.DirectMessage(f, msg).encode).ignore
          case _ => ZIO.unit
        }

      } yield ServerMessage.Acknowledge("sendDirectMessage")

object SendDirectMessageLive:
  val layer:ZLayer[TRef[ServerState] & SocketServer, Nothing, SendDirectMessage] = ZLayer.fromFunction(SendDirectMessageLive.apply)

