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
import chatrooms.domain.Acknowledge
import chatrooms.domain.SMError
import chatrooms.domain.ServerState
import chatrooms.domain.Client
import chatrooms.domain.SEAlreadyJoined
import chatrooms.domain.UserName
import chatrooms.domain.SMDirectMessage
import chatrooms.usecases.SendDirectMessage as SendDirectMessageUC
import chatrooms.usecases.SendDirectMessageLive

case class ServerConfig(port:Int)

def join (s:ServerState, name:UserName, clientId:ClientId) =

  val has = s.clients.exists((_, c) => c.id == clientId || c.name == name)
  val s1 = if has then s else s.addClient(Client(clientId, name))
  var response = if has then chatrooms.domain.SMError(SEAlreadyJoined()) else Acknowledge("join")
  println((s, s1).toString)
  (response, s1)
object Server extends ZIOAppDefault:

  def handleCommand (cmd:Command, clientId:ClientId) = cmd.match {
    case Command.JoinRoom(roomName) => for {
      c <- ZStream.succeed(WebSocketFrame.text(Acknowledge("joinRoom").encode))
    } yield c
    case Command.SendDirectMessage(to, msg) => for {
      _ <- ZStream.fromZIO( for {
        useCase <- ZIO.service[SendDirectMessageUC]
        c <- useCase.sendDirectMessage(clientId, to, msg)
      } yield c)
      c <- ZStream.succeed(WebSocketFrame.text(Acknowledge("sendDirectMessage").encode))
    } yield c
    case Command.Join(userName) => for {
      state <- ZStream.fromZIO(ZIO.service[TRef[ServerState]])
      msg <- ZStream.fromZIO(state.modify(join(_, userName, clientId)).commit)
      c <- ZStream.succeed(WebSocketFrame.text(msg.encode))
    } yield c
    case _ => ZStream.empty
  }

  def onConnect (state:TRef[ServerState]):CallbackE[SocketServer & SendDirectMessageUC & TRef[ServerState]] = {
      case x@(clientId, WebSocketFrame.Text(txt)) => for {
        _ <- ZStream.fromZIO(zio.Console.printLine("message received" ++ txt).ignore)
        x <- Command.parse(txt).match {
          case Some(cmd) => handleCommand(cmd, clientId)
          case None =>
            ZStream.execute(zio.Console.printLine(s"Cannot parse Message from ${txt}".format(txt)).ignore)
        }
      } yield x
  }

  def mkLayers (cfg:ServerConfig) = (
    SocketServer.liveConfigWithPort(cfg.port)
    >+> ZLayer.fromZIO(TRef.make[ServerState](ServerState.empty()).commit)
    >+> SocketServer.live
    >+> SendDirectMessageLive.layer
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
