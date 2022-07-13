package chatrooms

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
import chatrooms.usecases.SendMessageToRoomMock
import chatrooms.usecases.ListRoomsMock

val name = UserName("Peter")
val roomName = RoomName("MyRoom")
val clientId = ClientId("abc")

object CommandHandlerSpec extends ZIOSpecDefault {

  private val spec_ = suite("CommandHandlerSpec")(
    suite("handleCommand should")(
      test("call Join.join when provided a Join command") {
        val expectedServerMessage = ServerMessage.Acknowledge("join")
        val mockJoin = JoinMock.Run(equalTo((clientId, name)), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.Join(name), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(ListRoomsMock.empty >+>SendMessageToRoomMock.empty >+> ListRoomMembersMock.empty >+> SendDirectMessageMock.empty >+> mockJoin >+> JoinRoomMock.empty >+> CommandHandlerLive.layer)
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      } +
      test("call SendDirectMessage.run when provided a SendDirectMessage command") {
        val expectedServerMessage = ServerMessage.Acknowledge("sendDirectMessage")
        val msg = "txt"
        val mockSendDirectMessage = SendDirectMessageMock.Run(
          equalTo((clientId, name, msg)), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.SendDirectMessage(name, msg), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(ListRoomsMock.empty >+> SendMessageToRoomMock.empty >+> ListRoomMembersMock.empty >+> mockSendDirectMessage >+> JoinMock.empty >+> JoinRoomMock.empty >+> CommandHandlerLive.layer)
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
      +
      test("call JoinRoom.run when provided a JoinRoom command") {
        val expectedServerMessage = ServerMessage.Acknowledge("joinRoom")
        val joinRoomMock = JoinRoomMock.Run(
          equalTo((clientId, roomName)), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.JoinRoom(roomName), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(ListRoomsMock.empty >+> SendMessageToRoomMock.empty >+> ListRoomMembersMock.empty >+> SendDirectMessageMock.empty >+> JoinMock.empty >+> joinRoomMock >+> CommandHandlerLive.layer)
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
      +
      test("call ListRoomMembers.run when provided a ListRoomMembers command") {
        val expectedServerMessage = ServerMessage.Acknowledge("listRoomMembers")
        val listRoomMembersMock = ListRoomMembersMock.Run(
          equalTo((clientId, roomName)), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.ListRoomMembers(roomName), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(ListRoomsMock.empty >+> SendMessageToRoomMock.empty >+> JoinRoomMock.empty >+> SendDirectMessageMock.empty >+> JoinMock.empty >+> listRoomMembersMock >+> CommandHandlerLive.layer)
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
      +
      test("call SendMessageToRoom.run when provided a SendMessageToRoom command") {
        val msg = "txt"
        val sendMessageToRoomMock = SendMessageToRoomMock.Run(
          equalTo((clientId, roomName, msg)), value(None)).toLayer
        val app = CommandHandler.handleCommand(Command.SendMessageToRoom(roomName, msg), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(ListRoomsMock.empty >+> sendMessageToRoomMock >+> JoinRoomMock.empty >+> SendDirectMessageMock.empty >+> JoinMock.empty >+> ListRoomMembersMock.empty >+> CommandHandlerLive.layer)
        assertZIO(out)(equalTo(List.empty))
      }
      +
      test("call ListRooms.run when provided a ListRooms command") {
        val msg = "txt"
        val expectedServerMessage = ServerMessage.AllRoomNames(Set.empty)
        val listRoomsMock = ListRoomsMock.Run(
          equalTo(clientId), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.ListRooms, clientId).runCollect.map(_.toList)
        val out = app.provideLayer(listRoomsMock >+> SendMessageToRoomMock.empty >+> JoinRoomMock.empty >+> SendDirectMessageMock.empty >+> JoinMock.empty >+> ListRoomMembersMock.empty >+> CommandHandlerLive.layer)
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
    )
  ) @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}