package chatrooms.usecases

import zio.mock.Mock
import zio.mock.Proxy
import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.SocketServer
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage
import zio.stream.ZStream
import chatrooms.domain.Client
import chatrooms.usecases.SendDirectMessage as SendDirectMessageUC
import chatrooms.domain.ServerError
import zio.internal.stacktracer.Tracer

object MockSendDirectMessage extends Mock[SendDirectMessageUC]:
  object SendDirectMessage extends Effect[(ClientId, UserName, String), Nothing, ServerMessage]

  val compose: URLayer[Proxy, SendDirectMessageUC] = {
    ZLayer.fromZIO(
      ZIO.service[Proxy]
      .map {proxy =>
        new SendDirectMessageUC {
            def sendDirectMessage(from: ClientId, to: UserName, msg: String): ZIO[Any, Nothing, ServerMessage] = proxy(SendDirectMessage, from, to, msg)
        }
      }
    )
  }



