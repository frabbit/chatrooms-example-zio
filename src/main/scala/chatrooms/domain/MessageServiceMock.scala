package chatrooms.domain

import zio.mock.Mock
import zio.mock.Proxy
import zio.*
import zio.stm.TRef

import zio.stream.ZStream
import zio.internal.stacktracer.Tracer

object MessageServiceMock extends Mock[MessageService]:
  object SendTo extends Effect[(ClientId, ServerMessage), Nothing, Unit]

  val compose: URLayer[Proxy, MessageService] = {
    ZLayer.fromZIO(
      ZIO.service[Proxy]
      .map {proxy =>
        new MessageService {
          def sendTo(clientId:ClientId, msg:ServerMessage):ZIO[Any, Nothing, Unit] = proxy(SendTo, clientId, msg)
        }
      }
    )
  }



