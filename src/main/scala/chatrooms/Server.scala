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
import chatrooms.domain.SMError
import chatrooms.domain.JoinRoom
import chatrooms.domain.Join
import chatrooms.domain.ServerState
import chatrooms.domain.Client
import chatrooms.domain.SEAlreadyJoined
import chatrooms.domain.UserName
import chatrooms.domain.SendDirectMessage
import chatrooms.domain.SMDirectMessage

case class ServerConfig(port:Int)

def join (s:ServerState, name:UserName, clientId:ClientId) =

  val has = s.clients.exists((_, c) => c.id == clientId || c.name == name)
  val s1 = if has then s else s.addClient(Client(clientId, name))
  var response = if has then chatrooms.domain.SMError(SEAlreadyJoined()) else Acknowledge("join")
  println((s, s1).toString)
  (response, s1)
object Server extends ZIOAppDefault:

  def handleCommand (cmd:Command, clientId:ClientId, state:TRef[ServerState]) = cmd.match {
    case JoinRoom(roomName) => for {
      c <- ZStream.succeed(WebSocketFrame.text(Acknowledge("joinRoom").encode))
    } yield c
    case SendDirectMessage(to, msg) => for {
      _ <- ZStream.fromZIO( for {
        server <- ZIO.service[SocketServer]
        state <- state.get.commit
        receiverClientId = state.clients.find(_._2.name == to).map(_._2.id)
        from = state.clients.find(_._2.id == clientId).map(_._2.name)
        _ <- zio.Console.printLine("SendDirectMessage " ++ (receiverClientId, from).toString).ignore
        _ <- (from, receiverClientId).match {
          case (Some(f), Some(id)) => server.sendTo(id, SMDirectMessage(f, msg).encode).ignore
          case _ => ZIO.unit
        }
      } yield ())
      c <- ZStream.succeed(WebSocketFrame.text(Acknowledge("sendDirectMessage").encode))
    } yield c
    case Join(userName) => for {
      msg <- ZStream.fromZIO(state.modify(join(_, userName, clientId)).commit)
      c <- ZStream.succeed(WebSocketFrame.text(msg.encode))
    } yield c
    case _ => ZStream.empty
  }

  def onConnect (state:TRef[ServerState]):CallbackE[SocketServer] = {
      case x@(clientId, WebSocketFrame.Text(txt)) => for {
        _ <- ZStream.fromZIO(zio.Console.printLine("message received" ++ txt).ignore)
        x <- Command.parse(txt).match {
          case Some(cmd) => handleCommand(cmd, clientId, state)
          case None =>
            ZStream.execute(zio.Console.printLine(s"Cannot parse Message from ${txt}".format(txt)).ignore)
        }
      } yield x
  }

  def mkLayers (cfg:ServerConfig) = (SocketServer.liveConfigWithPort(cfg.port) >>> SocketServer.live).memoize


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
