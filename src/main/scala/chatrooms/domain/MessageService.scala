package chatrooms.domain

import zio.*

trait MessageService:
  def sendTo (clientId:ClientId, sm:ServerMessage):ZIO[Any, Nothing, Unit]