package chatrooms

import scala.language.postfixOps
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.ws.{WebSocket}
import zio._
import zio.Console._
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

object WebSocketClient extends ZIOAppDefault {

  def useWebSocket(ws: WebSocket[[X] =>> ZIO[Console, Throwable, X]]): RIO[Console, Unit] = {
    val receive = ws.receiveText().flatMap(t => Console.printLine(s"RECEIVED: $t"))
    ZIO.collectAllPar(List(
        ws.sendText(s":joinRoom myRoom"),
        receive,
    )).unit
  }

  // create a description of a program, which requires two dependencies in the environment:
  // the SttpClient, and the Console
  val sendAndPrint: RIO[Console with SttpClient, Response[Unit]] =
    sendR(basicRequest.get(uri"ws://127.0.0.1:8092").response(asWebSocketAlways(useWebSocket)))


  override def run =
    sendAndPrint.provide(AsyncHttpClientZioBackend.layer(), ZLayer.succeed(ConsoleLive))


}
