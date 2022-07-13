package chatrooms

import zio.test._
import zio.mock.Expectation.{unit, value, valueF}
import zio.test.Assertion.{equalTo }
import chatrooms.domain.Command
import chatrooms.domain.UserName
import chatrooms.CommandHandler
import chatrooms.domain.ClientId
import chatrooms.domain.ServerMessage
import chatrooms.usecases.JoinMock
import zio.ZLayer.apply
import zio.ZLayer
import chatrooms.usecases.SendDirectMessageMock
import zhttp.socket.WebSocketFrame

val name = UserName("Peter")
val clientId = ClientId("abc")


private val spec_ = suite("CommandHandlerSpec")(
  suite("handleCommand should")(
    test("call Join.join when provided a Join command") {
      val expectedServerMessage = ServerMessage.Acknowledge("join")
      val mockJoin = JoinMock.Run(equalTo((clientId, name)), value(expectedServerMessage)).toLayer
      val app = CommandHandler.handleCommand(Command.Join(name), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(SendDirectMessageMock.empty >+> mockJoin >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
    } +
    test("call SendDirectMessage.sendDirectMessage when provided a SendDirectMessage command") {
      val expectedServerMessage = ServerMessage.Acknowledge("sendDirectMessage")
      val msg = "txt"
      val mockSendDirectMessage = SendDirectMessageMock.Run(
        equalTo((clientId, name, msg)), value(expectedServerMessage)).toLayer
      val app = CommandHandler.handleCommand(Command.SendDirectMessage(name, msg), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(mockSendDirectMessage >+> JoinMock.empty >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(expectedServerMessage.encode))))
    }
  )
).provideSomeLayer(zio.Random.live).provideSomeLayer(zio.test.Sized.live(100))

object CommandHandlerSpec extends ZIOSpecDefault {
  override def spec = spec_
}