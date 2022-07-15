package chatrooms.socketapp

import zio.test._
import zio.mock.Expectation.{unit, value, valueF}
import zio.test.Assertion.{equalTo }
import chatrooms.domain.Command
import chatrooms.domain.UserName
import chatrooms.domain.RoomName
import chatrooms.socketapp.CommandHandler
import chatrooms.domain.ClientId
import chatrooms.domain.ServerMessage
import chatrooms.usecases.JoinMock
import zio.ZLayer.apply
import zio.ZLayer
import zio.ULayer
import chatrooms.usecases.SendDirectMessageMock
import zhttp.socket.WebSocketFrame
import chatrooms.usecases.JoinRoomMock
import chatrooms.usecases.ListRoomMembersMock
import chatrooms.usecases.SendMessageToRoomMock
import chatrooms.usecases.ListRoomsMock
import chatrooms.usecases.ListRooms
import chatrooms.usecases.SendMessageToRoom
import chatrooms.usecases.ListRoomMembers
import chatrooms.usecases.SendDirectMessage
import chatrooms.usecases.Join
import chatrooms.usecases.JoinRoom

val name = UserName("Peter")
val roomName = RoomName("MyRoom")
val clientId = ClientId("abc")

object CommandHandlerLiveSpec extends ZIOSpecDefault {

  def dependencyLayer (
    listRooms:ULayer[ListRooms] = ListRoomsMock.empty,
    sendMessageToRoom:ULayer[SendMessageToRoom] = SendMessageToRoomMock.empty,
    listRoomMembers:ULayer[ListRoomMembers] = ListRoomMembersMock.empty,
    sendDirectMessage:ULayer[SendDirectMessage] = SendDirectMessageMock.empty,
    join:ULayer[Join] = JoinMock.empty,
    joinRoom:ULayer[JoinRoom] = JoinRoomMock.empty
  ) =
        listRooms
    >+> sendMessageToRoom
    >+> listRoomMembers
    >+> sendDirectMessage
    >+> join
    >+> joinRoom
    >+> CommandHandlerLive.layer

  private val spec_ = suite("CommandHandlerLiveSpec")(
    suite("handleCommand should")(
      test("call Join.join when provided a Join command") {
        val expectedServerMessage = ServerMessage.Acknowledge("join")
        val mock = JoinMock.Run(equalTo((clientId, name)), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.Join(name), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(dependencyLayer(join = mock))
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      } +
      test("call SendDirectMessage.run when provided a SendDirectMessage command") {
        val expectedServerMessage = ServerMessage.Acknowledge("sendDirectMessage")
        val msg = "txt"
        val mock = SendDirectMessageMock.Run(
          equalTo((clientId, name, msg)), value(expectedServerMessage)).toLayer
        val sendDirectMessage = mock
        val app = CommandHandler.handleCommand(Command.SendDirectMessage(name, msg), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(dependencyLayer(sendDirectMessage = mock))
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
      +
      test("call JoinRoom.run when provided a JoinRoom command") {
        val expectedServerMessage = ServerMessage.Acknowledge("joinRoom")
        val mock = JoinRoomMock.Run(
          equalTo((clientId, roomName)), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.JoinRoom(roomName), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(dependencyLayer(joinRoom = mock))
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
      +
      test("call ListRoomMembers.run when provided a ListRoomMembers command") {
        val expectedServerMessage = ServerMessage.Acknowledge("listRoomMembers")
        val mock = ListRoomMembersMock.Run(
          equalTo((clientId, roomName)), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.ListRoomMembers(roomName), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(dependencyLayer(listRoomMembers = mock))
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
      +
      test("call SendMessageToRoom.run when provided a SendMessageToRoom command") {
        val msg = "txt"
        val mock = SendMessageToRoomMock.Run(
          equalTo((clientId, roomName, msg)), value(None)).toLayer
        val app = CommandHandler.handleCommand(Command.SendMessageToRoom(roomName, msg), clientId).runCollect.map(_.toList)
        val out = app.provideLayer(dependencyLayer(sendMessageToRoom = mock))
        assertZIO(out)(equalTo(List.empty))
      }
      +
      test("call ListRooms.run when provided a ListRooms command") {
        val msg = "txt"
        val expectedServerMessage = ServerMessage.AllRoomNames(Set.empty)
        val mock = ListRoomsMock.Run(
          equalTo(clientId), value(expectedServerMessage)).toLayer
        val app = CommandHandler.handleCommand(Command.ListRooms, clientId).runCollect.map(_.toList)
        val out = app.provideLayer(dependencyLayer(listRooms = mock))
        assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
      }
    )
  ) @@ chatrooms.CustomTestConfig.use

  override def spec = spec_
}