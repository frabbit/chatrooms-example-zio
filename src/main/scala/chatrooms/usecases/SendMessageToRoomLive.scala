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
import chatrooms.domain.MessageService

final case class SendMessageToRoomLive(stateRef:TRef[ServerState], ms:MessageService) extends SendMessageToRoom:

  def run(clientId:ClientId, roomName: RoomName, txt:String): ZIO[Any, Nothing, Option[ServerMessage]] = for {
    state <- stateRef.get.commit
    clientIdsOpt = state.getClientIdsOfRoom(roomName)
    senderInRoom = clientIdsOpt.map(_.exists(_ == clientId))
    userName = state.getClientName(clientId)
    x <- (senderInRoom, userName, clientIdsOpt).match {
      case (Some(false), _, _) =>
        ZIO.succeed(Some(ServerMessage.Error(ServerError.UserIsNotInRoom)))
      case (_, Some(name), Some(clientIds)) =>
        val msg = ServerMessage.RoomMessage(name, roomName, txt)
        ZIO.collectAll(clientIds.map(ms.sendTo(_, msg))) *> ZIO.succeed(None)
      case _ =>
        ZIO.succeed(None)
    }
  } yield x


object SendMessageToRoomLive:
  val layer:ZLayer[TRef[ServerState] & MessageService, Nothing, SendMessageToRoom] = ZLayer.fromFunction(SendMessageToRoomLive.apply)

