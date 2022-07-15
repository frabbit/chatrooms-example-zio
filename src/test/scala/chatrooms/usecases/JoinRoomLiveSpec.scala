package chatrooms.usecases

import zio.ZIO
import zio.test._
import zio.mock.Expectation.{unit, value, valueF}
import zio.test.Assertion.{equalTo }
import chatrooms.domain.Command
import chatrooms.domain.UserName
import chatrooms.domain.RoomName
import chatrooms.domain.ClientId
import chatrooms.domain.ServerMessage
import chatrooms.usecases.JoinMock
import zio.ZLayer.apply
import zio.ZLayer
import chatrooms.usecases.SendDirectMessageMock
import zhttp.socket.WebSocketFrame
import chatrooms.usecases.JoinRoomMock
import chatrooms.usecases.ListRoomMembersMock
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ServerError
import chatrooms.domain.Generators

object JoinRoomLiveSpec extends ZIOSpecDefault {

  private def fromState [B](f:ServerState => B):ZIO[TRef[ServerState], Nothing, B] = ZIO.serviceWithZIO[TRef[ServerState]](_.get.commit).map(f)
  private def stateLayer (s:ServerState = ServerState.empty()) = ZLayer.fromZIO(TRef.make(s).commit)

  private def deps(state:ServerState = ServerState.empty()) = stateLayer(state) >+> JoinRoomLive.layer

  private val spec_ = suite("JoinLiveSpec.run should")(
    test("return a ServerMessage containing an error when user has not joined the server") {
      check(Generators.clientId, Generators.roomName) { (clientId, roomName) =>
        val app = JoinRoom.run(clientId, roomName)
        assertZIO(app.provideLayer(deps()))(equalTo(ServerMessage.Error(ServerError.NotJoined)))
      }
    }
    +
    test("create the room when it does not exist") {
      check(Generators.client, Generators.roomName) { (client, roomName) =>
        val Right(initialState) = ServerState.empty().addClient(client)
        val app = JoinRoom.run(client.id, roomName) *> fromState(_.getAllRoomNames)
        assertZIO(app.provideLayer(deps(state = initialState)))(equalTo(Set(roomName)))
      }
    }
    +
    test("join the room when it does exist") {
      check(Generators.client, Generators.client, Generators.roomName) { (clientA, clientB, roomName) =>
        val Right(initialState) = ServerState.empty().addClient(clientA).flatMap(_.addClient(clientB)).flatMap(_.joinRoom(roomName, clientA.id))
        val app = JoinRoom.run(clientB.id, roomName) *> fromState(_.getRoomMemberNames(roomName))
        assertZIO(app.provideLayer(deps(state = initialState)))(equalTo(Some(Set(clientA.name, clientB.name))))
      }
    }
  )  @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}