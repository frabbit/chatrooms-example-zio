package chatrooms.usecases

import zio.mock.Mock
import zio.mock.Proxy
import zio.*
import chatrooms.domain.ClientId
import chatrooms.domain.ServerMessage

object ListRoomsMock extends Mock[ListRooms]:
  object Run extends Effect[ClientId, Nothing, ServerMessage]

  val compose: URLayer[Proxy, ListRooms] = {
    ZLayer.fromZIO(
      ZIO.service[Proxy]
      .map {proxy =>
        new ListRooms {
          def run(clientId:ClientId):ZIO[Any, Nothing, ServerMessage] = proxy(Run, clientId)
        }
      }
    )
  }



