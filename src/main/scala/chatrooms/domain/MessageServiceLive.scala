package chatrooms.domain

import zio.*
import zio.stm.TRef
import zio.stream.ZStream

final case class MessageServiceLive(server:SocketServer) extends MessageService:

  def sendTo (clientId:ClientId, sm:ServerMessage):ZIO[Any, Nothing, Unit] =
    server.sendTo(clientId, sm.encode).ignore *> ZIO.unit

object MessageServiceLive:
  val layer:ZLayer[SocketServer, Nothing, MessageService] = ZLayer.fromFunction(MessageServiceLive.apply)

