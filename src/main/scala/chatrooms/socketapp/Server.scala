package chatrooms.socketapp

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
import chatrooms.socketapp.SocketServer
import chatrooms.socketapp.{Callback, CallbackE}
import zio.ZLayer
import zio.Scope
import chatrooms.domain.Command
import chatrooms.domain.ServerState
import chatrooms.domain.Client
import chatrooms.domain.UserName
import chatrooms.socketapp.MessageServiceLive

import chatrooms.usecases.SendDirectMessage
import chatrooms.usecases.SendDirectMessageLive
import chatrooms.usecases.JoinRoomLive
import chatrooms.usecases.ListRoomsLive
import chatrooms.usecases.ListRoomMembersLive
import chatrooms.domain.ServerMessage
import chatrooms.domain.ServerError
import chatrooms.socketapp.SocketServer
import chatrooms.socketapp.CallbackE
import chatrooms.socketapp.SocketServerConfig
import chatrooms.usecases.Join
import chatrooms.usecases.JoinLive
import chatrooms.usecases.SendMessageToRoomLive
import chatrooms.socketapp.CommandHandlerLive
import chatrooms.socketapp.CommandHandler

case class ServerConfig(port:Int)

object Server extends ZIOAppDefault:

  def onConnect (state:TRef[ServerState]):CallbackE[CommandHandler] = {
      case x@(clientId, WebSocketFrame.Text(txt)) => for {
        // _ <- ZStream.fromZIO(zio.Console.printLine("message received" ++ txt).ignore)
        x <- Command.parse(txt).match {
          case Some(cmd) => CommandHandler.handleCommand(cmd, clientId)
          case None =>
            ZStream.execute(zio.Console.printLine(s"Cannot parse Message from ${txt}".format(txt)).ignore)
        }
      } yield x
  }

  def mkLayers (cfg:ServerConfig) = (
    SocketServerConfig.withPortLayer(cfg.port)
    >+> ZLayer.fromZIO(TRef.make[ServerState](ServerState.empty()).commit)
    >+> SocketServer.live
    >+> MessageServiceLive.layer
    >+> SendDirectMessageLive.layer
    >+> JoinLive.layer
    >+> JoinRoomLive.layer
    >+> ListRoomMembersLive.layer
    >+> SendMessageToRoomLive.layer
    >+> ListRoomsLive.layer
    >+> CommandHandlerLive.layer
  ).memoize

  def app (cfg:ServerConfig): ZIO[Any, Nothing, Unit] = for {
    map <- TRef.make(Map()).commit
    state <- TRef.make(ServerState.empty()).commit
    layers <- mkLayers(cfg).provideLayer(Scope.default)
    _ <-
      SocketServer.start({
        case t if (onConnect(state).isDefinedAt(t)) => onConnect(state)(t).provideLayer(layers)
      }).provideLayer(layers)

  } yield ()

  override def run = app(ServerConfig(8092))

end Server
