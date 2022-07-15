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
import zio.ZLayer.apply
import zio.ZLayer
import zio.stm.TRef
import chatrooms.domain.ServerState
import chatrooms.domain.ServerError
import chatrooms.domain.Generators
import chatrooms.domain.Client

object ListRoomMembersLiveSpec extends ZIOSpecDefault {

  private def fromState [B](f:ServerState => B):ZIO[TRef[ServerState], Nothing, B] = ZIO.serviceWithZIO[TRef[ServerState]](_.get.commit).map(f)
  private def stateLayer (s:ServerState = ServerState.empty()) = ZLayer.fromZIO(TRef.make(s).commit)

  private def deps(state:ServerState = ServerState.empty()) = stateLayer(state) >+> ListRoomMembersLive.layer

  private val spec_ = suite("ListRoomMembersLiveSpec.run should")(
    test("return a ServerMessage containing an Error when client has not joined") {
      check(Generators.clientId, Generators.roomName) { (clientId, roomName) =>
        val app = ListRoomMembers.run(clientId, roomName)
        assertZIO(app.provideLayer(deps()))(equalTo(ServerMessage.Error(ServerError.NotJoined)))
      }
    } +
    test("return a ServerMessage containing an Error when room does not exist") {
      check(Generators.client, Generators.roomName) { (client, roomName) =>
        val Right(initialState) = ServerState.empty().addClient(client)
        val app = ListRoomMembers.run(client.id, roomName)
        assertZIO(app.provideLayer(deps(state = initialState)))(equalTo(ServerMessage.Error(ServerError.RoomNotFound(roomName))))
      }
    }
    +
    test("return a ServerMessage containing an Error when room exists but client is not in room") {
      check(Generators.client, Generators.client, Generators.roomName) { (clientA, clientB, roomName) =>
        val Right(initialState) = Right(ServerState.empty())
          .flatMap(_.addClient(clientA))
          .flatMap(_.addClient(clientB))
          .flatMap(_.joinRoom(roomName, clientA.id))
        val app = ListRoomMembers.run(clientB.id, roomName)
        assertZIO(app.provideLayer(deps(state = initialState)))(equalTo(ServerMessage.Error(ServerError.UserIsNotInRoom)))
      }
    } +
    test("return a list containing himself when room exists and client is the only member") {
      check(Generators.client, Generators.roomName) { (client, roomName) =>
        val Right(initialState) = Right(ServerState.empty())
          .flatMap(_.addClient(client))
          .flatMap(_.joinRoom(roomName, client.id))
        val app = ListRoomMembers.run(client.id, roomName)
        assertZIO(app.provideLayer(deps(state = initialState)))(equalTo(ServerMessage.AllRoomMembers(roomName, Set(client.name))))
      }
    } +
    test("return a list of multiple user names when room exists and client is one of multiple members") {
      check(Generators.client, Generators.clientList, Generators.roomName) { (client, otherClients, roomName) =>
        val state = Right(ServerState.empty())
          .flatMap(_.addClient(client))
          .flatMap(_.joinRoom(roomName, client.id))
        val Right(initialState) = otherClients.foldLeft(state)((acc, c) => acc.flatMap(_.addClient(c)).flatMap(_.joinRoom(roomName, c.id)))
        val app = ListRoomMembers.run(client.id, roomName)
        assertZIO(app.provideLayer(deps(state = initialState)))(equalTo(ServerMessage.AllRoomMembers(roomName, Set.from(otherClients.map(_.name)) + client.name )))
      }
    }
  ) @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}