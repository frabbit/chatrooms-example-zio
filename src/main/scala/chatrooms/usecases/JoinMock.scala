package chatrooms.usecases

import zio.mock.Mock
import zio.mock.Proxy
import zio.*
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ClientId
import chatrooms.domain.UserName
import chatrooms.domain.ServerMessage
import zio.stream.ZStream
import chatrooms.domain.Client
import chatrooms.domain.ServerError
import zio.internal.stacktracer.Tracer

object JoinMock extends Mock[Join]:
  object Run extends Effect[(ClientId, UserName), Nothing, ServerMessage]

  val compose: URLayer[Proxy, Join] = {
    ZLayer.fromZIO(
      ZIO.service[Proxy]
      .map {proxy =>
        new Join {
          def run(clientId:ClientId, name:UserName):ZIO[Any, Nothing, ServerMessage] = proxy(Run, clientId, name)
        }
      }
    )
  }
