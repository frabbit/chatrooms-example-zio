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
import chatrooms.domain.Command
import chatrooms.domain.JoinRoom.apply
import chatrooms.domain.Acknowledge
import chatrooms.domain.JoinRoom

object Server extends ZIOAppDefault:

  def handleCommand (cmd:Command) = cmd.match {
    case JoinRoom(roomName) => ZStream.succeed(WebSocketFrame.text(Acknowledge("joinRoom").encode))
    case _ => ZStream.empty
  }
  def onConnect:CallbackE[SocketServer] = {
      case x@(_, WebSocketFrame.Text(txt)) => for {
        _ <- ZStream.fromZIO(zio.Console.printLine("message received" ++ txt).ignore)
        x <- Command.parse(txt).match {
          case Some(cmd) => handleCommand(cmd)
          case None =>
            ZStream.execute(zio.Console.printLine(s"Cannot parse Message from ${txt}".format(txt)).ignore)
        }
      } yield x
  }

  val mkLayers = (SocketServer.liveConfig >>> SocketServer.live).memoize


  val app: ZIO[Any, Nothing, Unit] = for {
    map <- TRef.make(Map()).commit
    layers <- mkLayers.provideLayer(Scope.default)
    _ <-
      SocketServer.start({
        case t if (onConnect.isDefinedAt(t)) => onConnect(t).provideLayer(layers)
      }).provideLayer(layers)

  } yield ()

  override def run = app

end Server
