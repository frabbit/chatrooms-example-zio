package chatrooms

import zio.test._
import zio.mock.Expectation.{unit, value, valueF}
import zio.test.Assertion.{equalTo }
import chatrooms.domain.Command
import chatrooms.domain.UserName
import chatrooms.CommandHandler
import chatrooms.domain.ClientId
import chatrooms.domain.ServerMessage
import chatrooms.usecases.MockJoin
import zio.ZLayer.apply
import zio.ZLayer
import chatrooms.usecases.MockSendDirectMessage
import zhttp.socket.WebSocketFrame

val name = UserName("Peter")
val clientId = ClientId("abc")


private val spec_ = suite("CommandHandlerSpec")(
  suite("handleCommand should")(
    test("call Join.join when provided a Join command") {
      val mockJoin = MockJoin.Run(equalTo((clientId, name)), value(ServerMessage.Acknowledge("join"))).toLayer
      val app = CommandHandler.handleCommand(Command.Join(name), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(MockSendDirectMessage.empty >+> mockJoin >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(ServerMessage.Acknowledge("join").encode))))
    } +
    test("call SendDirectMessage.sendDirectMessage when provided a SendDirectMessage command") {
      val msg = "txt"
      val mockSendDirectMessage = MockSendDirectMessage.Run(
        equalTo((clientId, name, msg)), value(ServerMessage.Acknowledge("sendDirectMessage"))).toLayer
      val app = CommandHandler.handleCommand(Command.SendDirectMessage(name, msg), clientId).runCollect.map(_.toList)
      val out = app.provideLayer(mockSendDirectMessage >+> MockJoin.empty >>> CommandHandlerLive.layer)
      assertZIO(out)(equalTo(List(WebSocketFrame.text(ServerMessage.Acknowledge("sendDirectMessage").encode))))
    }
  )
).provideSomeLayer(zio.Random.live).provideSomeLayer(zio.test.Sized.live(100))

object CommandHandlerSpec extends ZIOSpecDefault {
  override def spec = spec_
}