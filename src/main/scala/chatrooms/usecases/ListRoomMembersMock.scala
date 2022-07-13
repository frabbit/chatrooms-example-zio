package chatrooms.usecases

import zio.mock.Mock
import zio.mock.Proxy
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
import zio.internal.stacktracer.Tracer

object ListRoomMembersMock extends Mock[ListRoomMembers]:
  object Run extends Effect[(ClientId, RoomName), Nothing, ServerMessage]

  val compose: URLayer[Proxy, ListRoomMembers] = {
    ZLayer.fromZIO(
      ZIO.service[Proxy]
      .map {proxy =>
        new ListRoomMembers {
          def run(clientId:ClientId, name:RoomName):ZIO[Any, Nothing, ServerMessage] = proxy(Run, clientId, name)
        }
      }
    )
  }



