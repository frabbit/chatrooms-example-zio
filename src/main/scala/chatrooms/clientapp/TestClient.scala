package chatrooms.clientapp

import scala.language.postfixOps
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.ws.{WebSocket}
import zio._
import zio.stm.TQueue
import zio.Console._
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import sttp.ws.WebSocketFrame
import scala.concurrent.duration.Duration.apply
import zio.stm.TRef
import chatrooms.socketapp.ServerConfig

type Send[Msg] = Option[Msg] => UIO[Unit]

type WS = WebSocket[[X] =>> ZIO[Console, Throwable, X]]
type Callback = String => ZIO[Any, Nothing, Unit]

type CallbackTyped[Msg] = Msg => ZIO[Any, Nothing, Unit]

object TestClient {

  def receiveLoop (ws: WS, callback: Callback): ZIO[Console, Throwable, Unit] =
    val receive = ws.receive()
    for {
      _ <- receive.either.flatMap {
        case Right(WebSocketFrame.Text(m, finalF, rsv)) =>
          //Console.printLine("text message received: " ++ m).ignore *>
          callback(m)
        case Right(r) =>
          Console.printLine("other received: " ++ r.toString()).ignore
        case Left(e) =>
          Console.printLine("error received: " ++ e.toString).ignore
      }
      _ <- receiveLoop(ws, callback)
    } yield ()

  def sendLoop (ws:WS, queue:TQueue[Option[String]]):RIO[Console, Unit] =
    queue.take.commit.flatMap {
      case Some(m) =>
        ws.sendText(m).either.flatMap {
          case Right(x) => ZIO.unit
          case Left(x) => Console.printLine(
            "sendTextResult Failure: " ++ m.toString() ++ ": " ++ x.toString()).ignore
        }
        *> sendLoop(ws, queue)
      case None =>
        ZIO.unit
    }

  def mainLoop (ws:WS,callback:Callback, queue:TQueue[Option[String]]):RIO[Console, Unit] =
    for {
      x <- receiveLoop(ws, callback).fork
      _ <- sendLoop(ws, queue)
      _ <- x.interrupt
    } yield ()

  def start (callback:Callback, queue:TQueue[Option[String]], cfg:ServerConfig): RIO[Console with SttpClient, Response[Unit]] =
    sendR(basicRequest.get(uri"ws://127.0.0.1:${cfg.port}")
      .response(asWebSocketAlways((x:WS) => mainLoop(x, callback, queue))))

  def createClient (callback:Callback, cfg:ServerConfig) =
    def mkSend (queue:TQueue[Option[String]]) =
      (s:Option[String]) => queue.offer(s).commit.ignore
    for {
      queue <- TQueue.unbounded[Option[String]].commit
      clientFiber <- start(callback, queue, cfg).fork
      _ <- ZIO.sleep(200.milliseconds)
    } yield (mkSend(queue), clientFiber)

  def createTypedClient [Msg, Cmd](callbackTyped:CallbackTyped[Msg], commandEncoder: Cmd => String, messageDecoder: String => Option[Msg], cfg:ServerConfig) =
    val callback = (s:String) =>
      val msg = messageDecoder(s)
      msg.match {
        case Some(msg) => callbackTyped(msg)
        case None => ZIO.unit
      }
    createClient(callback, cfg)
      .map(
        (send, clientFiber) => ((x:Option[Cmd]) => send(x.map(commandEncoder)), clientFiber))
}
