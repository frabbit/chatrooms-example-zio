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
import chatrooms.usecases.Join as JoinUC
import chatrooms.domain.ServerError
import zio.internal.stacktracer.Tracer

object MockJoin extends Mock[JoinUC]:
  object Join extends Effect[(ClientId, UserName), Nothing, ServerMessage]

  val compose: URLayer[Proxy, JoinUC] = {
    ZLayer.fromZIO(
      ZIO.service[Proxy]
      .map {proxy =>
        new JoinUC {
            def join(clientId:ClientId, name:UserName):ZIO[Any, Nothing, ServerMessage] = proxy(Join, clientId, name)
        }
      }
    )
  }



