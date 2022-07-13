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

object ListRoomsLiveSpec extends ZIOSpecDefault {

  private def fromState [B](f:ServerState => B):ZIO[TRef[ServerState], Nothing, B] = ZIO.serviceWithZIO[TRef[ServerState]](_.get.commit).map(f)
  private def stateLayer (s:ServerState = ServerState.empty()) = ZLayer.fromZIO(TRef.make(s).commit)

  private def deps(s:ServerState = ServerState.empty()) = stateLayer(s) >+> ListRoomsLive.layer

  private val spec_ = suite("ListRoomsLiveSpec.run should")(
    test("return an empty set when no rooms exist") {
      check(Generators.clientId, Generators.userName) { (clientId, name) =>
        val app = ListRooms.run(clientId)
        assertZIO(app.provideLayer(deps()))(equalTo(ServerMessage.AllRoomNames(Set.empty)))
      }
    }
    +
    test("return a list of existing rooms") {
      check(Generators.clientId, Generators.userName) { (clientId, name) =>
        val roomA = RoomName("roomA")
        val roomB = RoomName("roomB")
        val Right(st) = ServerState.empty()
          .addClient(Client(clientId, name))
          .map(_.joinRoom(roomA, clientId))
          .map(_.joinRoom(roomB, clientId))
        val app = ListRooms.run(clientId)
        assertZIO(app.provideLayer(deps(st)))(equalTo(ServerMessage.AllRoomNames(Set(roomA, roomB))))
      }
    }
  ) @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}