package chatrooms.usecases

import zio.ZIO
import zio.test._
import zio.mock.Expectation.{unit, value, valueF}
import zio.test.Assertion.{equalTo }
import chatrooms.domain.Command
import chatrooms.domain.UserName
import chatrooms.domain.RoomName
import chatrooms.CommandHandler
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

object JoinLiveSpec extends ZIOSpecDefault {

  private def fromState [B](f:ServerState => B):ZIO[TRef[ServerState], Nothing, B] = ZIO.serviceWithZIO[TRef[ServerState]](_.get.commit).map(f)
  private def stateLayer (s:ServerState = ServerState.empty()) = ZLayer.fromZIO(TRef.make(s).commit)

  private def deps(s:ServerState = ServerState.empty()) = stateLayer(s) >+> JoinLive.layer

  private val spec_ = suite("JoinLiveSpec.run should")(
    test("add the client to the state") {
      check(Generators.clientId, Generators.userName) { (clientId, name) =>
        val app = Join.run(clientId, name) *> fromState(_.getClientIds())
        assertZIO(app.provideLayer(deps()))(equalTo(Set(clientId)))
      }
    }
    +
    test("return an Acknowledgment on success") {
      check(Generators.clientId, Generators.userName) { (clientId, name) =>
        val app = Join.run(clientId, name)
        assertZIO(app.provideLayer(deps()))(equalTo(ServerMessage.Acknowledge("join")))
      }
    }
    +
    test("fail when client already exists") {
      check(Generators.clientId, Generators.userName) { (clientId, name) =>
        val app = Join.run(clientId, name) *> Join.run(clientId, name)
        assertZIO(app.provideLayer(deps()))(equalTo(ServerMessage.Error(ServerError.AlreadyJoined)))
      }
    }
    +
    test("fail when name is already taken") {
      check(Generators.clientId, Generators.clientId, Generators.userName) { (clientIdA, clientIdB, name) =>
        val app = Join.run(clientIdA, name) *> Join.run(clientIdB, name)
        assertZIO(app.provideLayer(deps()))(equalTo(ServerMessage.Error(ServerError.UserNameTaken)))
      }
    }
    +
    test("add multiple clients with different ids and names to the state") {
      check(Generators.clientId, Generators.clientId, Generators.userName, Generators.userName) { (clientIdA, clientIdB, nameA, nameB) =>
        val app = Join.run(clientIdA, nameA) *> Join.run(clientIdB, nameB) *> fromState(_.getClientIds())
        assertZIO(app.provideLayer(deps()))(equalTo(Set(clientIdA, clientIdB)))
      }
    }
  )  @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}