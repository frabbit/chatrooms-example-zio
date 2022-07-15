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

object SendDirectMessageLiveSpec extends ZIOSpecDefault {

  private def fromState [B](f:ServerState => B):ZIO[TRef[ServerState], Nothing, B] = ZIO.serviceWithZIO[TRef[ServerState]](_.get.commit).map(f)
  private def stateLayer (s:ServerState = ServerState.empty()) = ZLayer.fromZIO(TRef.make(s).commit)

  private def deps(
      state:ServerState = ServerState.empty(),
      mock:ULayer[MessageService] = MessageServiceMock.empty
    ) =
    stateLayer(state) >+> mock >+> SendDirectMessageLive.layer

  private val spec_ = suite("SendMessageToRoomLiveSpec.run should")(
    test("return an message containing an error when sending client has not joined first") {
      check(Generators.clientId, Generators.userName, Generators.message) { (clientId, receiverName, message) =>
        val mock = MessageServiceMock.empty
        val app = SendDirectMessage.run(clientId, receiverName, message)
        assertZIO(app.provideLayer(deps(mock = mock)))(equalTo(ServerMessage.Error(ServerError.NotJoined)))
      }
    } +
    test("return an message containing an error when receiving username can not be found") {
      check(Generators.clientId, Generators.userName, Generators.userName, Generators.message) { (clientId, senderName, receiverName, message) =>
        val Right(initialState) = ServerState.empty().addClient(Client(clientId, senderName))
        val mock = MessageServiceMock.empty
        val app = SendDirectMessage.run(clientId, receiverName, message)
        assertZIO(app.provideLayer(deps(state = initialState, mock = mock)))(equalTo(ServerMessage.Error(ServerError.UserNameNotFound)))
      }
    } +
    test("acknowledge the message when sending was successful") {
      check(Generators.client, Generators.client, Generators.message) { (sender, receiver, message) =>
        val Right(initialState) = ServerState.empty().addClient(sender).flatMap(_.addClient(receiver))
        val msg = ServerMessage.DirectMessage(sender.name, message)
        val mock = MessageServiceMock.SendTo(equalTo((receiver.id, msg)), value(())).toLayer
        val app = SendDirectMessage.run(sender.id, receiver.name, message)
        assertZIO(app.provideLayer(deps(state = initialState, mock = mock)))(equalTo(ServerMessage.Acknowledge("sendDirectMessage")))
      }
    }

  ) @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}

