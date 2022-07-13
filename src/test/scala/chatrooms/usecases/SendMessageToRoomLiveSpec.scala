package chatrooms.usecases

import zio.ULayer
import zio.ZIO
import zio.test._
import zio.mock.Expectation.{unit, value, valueF}
import zio.test.Assertion.{equalTo, anything }
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
import chatrooms.domain.Client
import chatrooms.domain.MessageService
import chatrooms.domain.MessageServiceMock

object SendMessageToRoomLiveSpec extends ZIOSpecDefault {

  private def fromState [B](f:ServerState => B):ZIO[TRef[ServerState], Nothing, B] = ZIO.serviceWithZIO[TRef[ServerState]](_.get.commit).map(f)
  private def stateLayer (s:ServerState = ServerState.empty()) = ZLayer.fromZIO(TRef.make(s).commit)

  private def deps(
      s:ServerState = ServerState.empty(),
      mock:ULayer[MessageService] = MessageServiceMock.empty
    ) =
    stateLayer(s) >+> mock >+> SendMessageToRoomLive.layer

  private val spec_ = suite("SendMessageToRoomLiveSpec.run should")(
    test("send a message to the user itself via MessageService") {
      check(Generators.clientId, Generators.roomName, Generators.userName, Generators.message) { (clientId, roomName, name, message) =>
        val Right(initialState) = ServerState.empty().addClient(Client(clientId, name)).map(_.joinRoom(roomName, clientId))
        val msg = ServerMessage.RoomMessage(name, roomName, message)
        val mock = MessageServiceMock.SendTo(equalTo(clientId, msg), value(())).toLayer
        val app = SendMessageToRoom.run(clientId, roomName, message)
        assertZIO(app.provideLayer(deps(initialState, mock)))(equalTo(None))
      }
    } +
    test("fail when sending clientId is not in the room") {
      check(Generators.clientId, Generators.userName, Generators.clientId, Generators.roomName, Generators.userName, Generators.message) { (senderId, senderName, clientId, roomName, name, message) =>
        val Right(initialState) =
          ServerState.empty()
          .addClient(Client(clientId, name))
          .flatMap(_.addClient(Client(senderId, senderName)))
          .map(_.joinRoom(roomName, clientId))
        val msg = ServerMessage.RoomMessage(name, roomName, message)
        val mock = MessageServiceMock.empty
        val app = SendMessageToRoom.run(senderId, roomName, message)
        assertZIO(app.provideLayer(deps(initialState, mock)))(equalTo(Some(ServerMessage.Error(ServerError.UserIsNotInRoom))))
      }
    } +
    test("send a message to all users of a room via MessageService") {
      check( Generators.clientId, Generators.userName, Generators.clientId, Generators.userName, Generators.roomName, Generators.message) { (clientIdA, nameA, clientIdB, nameB, roomName, message) =>
        val Right(initialState) = ServerState.empty()
          .addClient(Client(clientIdA, nameA))
          .map(_.joinRoom(roomName, clientIdA))
          .flatMap(_.addClient(Client(clientIdB, nameB)))
          .map(_.joinRoom(roomName, clientIdB))

        val msg = ServerMessage.RoomMessage(nameA, roomName, message)
        val mock = (
              MessageServiceMock.SendTo(equalTo(clientIdA, msg), value(())).exactly(1)
          and MessageServiceMock.SendTo(equalTo(clientIdB, msg), value(())).exactly(1)
        ) .toLayer

        val app = SendMessageToRoom.run(clientIdA, roomName, message)
        assertZIO(app.provideLayer(deps(initialState, mock)))(equalTo(None))
      }
    }
  ) @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}

