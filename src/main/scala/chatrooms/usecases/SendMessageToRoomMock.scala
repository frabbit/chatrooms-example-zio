package chatrooms.usecases

import zio.mock.Mock
import zio.mock.Proxy
import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.RoomName
import chatrooms.domain.ServerMessage
import zio.stream.ZStream
import chatrooms.domain.Client
import chatrooms.domain.ServerError
import zio.internal.stacktracer.Tracer

object SendMessageToRoomMock extends Mock[SendMessageToRoom]:
  object Run extends Effect[(ClientId, RoomName, String), Nothing, Option[ServerMessage]]

  val compose: URLayer[Proxy, SendMessageToRoom] = {
    ZLayer.fromZIO(
      ZIO.service[Proxy]
      .map {proxy =>
        new SendMessageToRoom {
          def run(clientId:ClientId, name:RoomName, msg:String):ZIO[Any, Nothing, Option[ServerMessage]] = proxy(Run, clientId, name, msg)
        }
      }
    )
  }
