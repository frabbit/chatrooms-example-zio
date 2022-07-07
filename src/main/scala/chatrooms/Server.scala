package chatrooms

import scala.language.postfixOps
import zhttp.http._
import zhttp.service.{Server as ZHttpServer}
import zhttp.socket.{Socket, WebSocketFrame}

import zio.Console._
import zio.stream.ZStream
import zio.stm._
import zio.{ZIOApp, ExitCode, Schedule, UIO, URIO, Duration, ZIO, Chunk, durationInt}
import zio.ZIOAppDefault
import chatrooms.domain.{ClientId}
import zio.stm.TRef
import chatrooms.domain.SocketServer
import chatrooms.domain.{Callback, CallbackE}
import zio.ZLayer
import zio.Scope

object Server extends ZIOAppDefault:

  def onConnect:CallbackE[SocketServer] = (x:(ClientId, WebSocketFrame)) =>  x._2.match {
      case WebSocketFrame.Text("FOO")  => ZStream.succeed(WebSocketFrame.text("BAR"))
      case WebSocketFrame.Text("BAR")  => ZStream.succeed(WebSocketFrame.text("FOO"))
      case WebSocketFrame.Text("MYID") => ZStream.succeed(WebSocketFrame.text(x._1.value))
      case WebSocketFrame.Text("MYIP") => for {
        h <- ZStream.fromZIO(SocketServer.getHost(x._1))
        x <- ZStream.succeed(WebSocketFrame.text(h))
      } yield x
      case fr @ WebSocketFrame.Text(_) =>
        ZStream.repeat(fr).schedule(Schedule.spaced(1.second)).take(10)
    }

  val mkLayers = (SocketServer.liveConfig >>> SocketServer.live).memoize


  val app: ZIO[Any, Nothing, Unit] = for {
    map <- TRef.make(Map()).commit
    layers <- mkLayers.provideLayer(Scope.default)

    _ <- SocketServer.start(t => onConnect(t).provideLayer(layers))
      .provideLayer(layers)

  } yield ()

  override def run = app

end Server
