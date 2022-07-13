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
import chatrooms.domain.MessageService

final case class SendMessageToRoomLive(stateRef:TRef[ServerState], ms:MessageService) extends SendMessageToRoom:

  def run(clientId:ClientId, roomName: RoomName, txt:String): ZIO[Any, Nothing, Unit] = for {
    state <- stateRef.get.commit
    clients = state.getClientIdsOfRoom(roomName)
    userName = state.getClientName(clientId)

    _ <- (userName, clients).match {
      case (Some(name), Some(clientIds)) =>
        val msg = ServerMessage.RoomMessage(name, roomName, txt)
        ZIO.collectAll(clientIds.map(ms.sendTo(_, msg))) *> ZIO.unit
      case _ =>
        ZIO.unit
    }
    x <- ZIO.unit
  } yield x


object SendMessageToRoomLive:
  val layer:ZLayer[TRef[ServerState] & MessageService, Nothing, SendMessageToRoom] = ZLayer.fromFunction(SendMessageToRoomLive.apply)

