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

val name = UserName("Peter")
val roomName = RoomName("MyRoom")
val clientId = ClientId("abc")


private val spec_ = suite("CommandHandlerSpec")(
  suite("handleCommand should")(
    test("call Join.join when provided a Join command") {
      val expectedServerMessage = ServerMessage.Acknowledge("join")
      val mockJoin = JoinMock.Run(equalTo((clientId, name)), value(expectedServerMessage)).toLayer
      val app = CommandHandler.handleCommand(Command.Join(name), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(ListRoomMembersMock.empty >+> SendDirectMessageMock.empty >+> mockJoin >+> JoinRoomMock.empty >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
    } +
    test("call SendDirectMessage.run when provided a SendDirectMessage command") {
      val expectedServerMessage = ServerMessage.Acknowledge("sendDirectMessage")
      val msg = "txt"
      val mockSendDirectMessage = SendDirectMessageMock.Run(
        equalTo((clientId, name, msg)), value(expectedServerMessage)).toLayer
      val app = CommandHandler.handleCommand(Command.SendDirectMessage(name, msg), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(ListRoomMembersMock.empty >+> mockSendDirectMessage >+> JoinMock.empty >+> JoinRoomMock.empty >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
    }
    +
    test("call JoinRoom.run when provided a JoinRoom command") {
      val expectedServerMessage = ServerMessage.Acknowledge("joinRoom")
      val joinRoomMock = JoinRoomMock.Run(
        equalTo((clientId, roomName)), value(expectedServerMessage)).toLayer
      val app = CommandHandler.handleCommand(Command.JoinRoom(roomName), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(ListRoomMembersMock.empty >+> SendDirectMessageMock.empty >+> JoinMock.empty >+> joinRoomMock >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
    }
    +
    test("call ListRoomMembers.run when provided a ListRoomMembers command") {
      val expectedServerMessage = ServerMessage.Acknowledge("listRoomMembers")
      val listRoomMembersMock = ListRoomMembersMock.Run(
        equalTo((clientId, roomName)), value(expectedServerMessage)).toLayer
      val app = CommandHandler.handleCommand(Command.ListRoomMembers(roomName), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(JoinRoomMock.empty >+> SendDirectMessageMock.empty >+> JoinMock.empty >+> listRoomMembersMock >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
    }
  )
).provideSomeLayer(zio.Random.live).provideSomeLayer(zio.test.Sized.live(100))

object CommandHandlerSpec extends ZIOSpecDefault {
  override def spec = spec_
}