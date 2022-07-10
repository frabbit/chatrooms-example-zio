package chatrooms
package domain

import scala.languageFeature.implicitConversions

import util.chaining.scalaUtilChainingOps
import _root_.chatrooms.all.*
import zio.*
import zio.stm.*
import zio.ZLayer
import zhttp.http.{Request, Response, Http, !!, /, Method, HttpDataExtension, HttpData, Headers, ->}
import zhttp.service.{Server}
import zhttp.socket.{Socket, WebSocketFrame, IsWebSocket}
import zio.stream.ZStream
import zio.Console._
import zhttp.http.Method
import zhttp.http.HttpDataExtension
import zhttp.socket.IsWebSocket
import java.util.UUID
import zio.stm.TRef
import zhttp.http.Path
import zhttp.socket.Socket.PartialCollect
import zhttp.service.Logging
import zhttp.service.EventLoopGroup
import zhttp.service.server.ServerChannelFactory


case class SocketServerConfig(port: Int)

type CallbackE[E] = PartialFunction[(ClientId, WebSocketFrame), ZStream[Any & E, Nothing, WebSocketFrame]]

type Callback = CallbackE[Any]

type Start[E] = (onConnect:Callback) => ZIO[Any & E, Nothing, Unit]

trait SocketServer {
  def start:Start[Any]
  def getHost (clientId:ClientId):ZIO[Any, Nothing, String]
  def sendTo (clientId:ClientId, message:String):ZIO[Any, Throwable, Unit]
}

type RequestLookup = Map[String, (Request, TQueue[String])]

class SocketServerImpl (lookupRef: TRef[RequestLookup], cfg:SocketServerConfig) extends SocketServer {

  def sendTo (clientId:ClientId, message:String):ZIO[Any, Throwable, Unit] =

    for {
      _ <- Console.printLine("sendTo called" ++ (clientId, message).toString).ignore
      lookup <- lookupRef.get.commit
      inboxOpt = lookup.find(_._1 == clientId.value).map(_._2._2)
      _ <- inboxOpt.match {
        case Some(inbox) => inbox.offer(message).commit *> ZIO.unit
        case None => ZIO.unit
      }
    } yield ()


  private def socket (id: String, callback : Callback):Socket[Any, Nothing, WebSocketFrame, WebSocketFrame] =
    val pf:PartialFunction[WebSocketFrame, ZStream[Any, Nothing, WebSocketFrame]] = {
      case x if callback.isDefinedAt((ClientId(id), x)) => callback((ClientId(id), x))
    }
    Socket.collect[WebSocketFrame](pf)

  private def app (callback: Callback) =
    Http.collectZIO[Request] {

      case r@(Method.GET -> Path(Vector(), true)) =>
        for {
          _ <- Console.printLine("SERVER received: " ++ r.method.toString ++ " : " ++ r.path.toList.mkString)
          uuid <- ZIO.succeed(UUID.randomUUID())
          inbox <- TQueue.unbounded[String].commit
          _ <- lookupRef.update(x => x + (uuid.toString -> (r, inbox)) ).commit
          inboxStream = for {
            msg <- ZStream.fromZIO(inbox.take.commit)
            x <- ZStream.succeed(WebSocketFrame.text(msg))
          } yield x
          commandStream = socket(uuid.toString, callback)
          x <- commandStream.merge(Socket.fromStream(inboxStream)).toResponse
        } yield x
      case r =>
        for {
          _ <- Console.printLine("SERVER received: " ++ r.method.toString ++ " : " ++ r.path.toList.mkString)
          x <- ZIO.succeed(Response.ok)
        } yield x
    }

  def start:Start[Any] = (onConnect:Callback) =>
    Server.start(cfg.port, app(onConnect)).exitCode *> ZIO.unit

  def getHost (clientId:ClientId) =
    for {
      req <- lookupRef.get.commit.map( _.get(clientId.value))
      host <- req.flatMap(_._1.host).map(_.toString).getOrElse("<nohost>") |> ZIO.succeed
      x <- host |> ZIO.succeed
    } yield x
}


object SocketServer {
  def start:Start[SocketServer] = (onConnect:Callback) => for {
    s <- ZIO.service[SocketServer]
    x <- s.start(onConnect)

  } yield x

  def getHost (clientId:ClientId):ZIO[SocketServer, Nothing, String] = for {
    s <- ZIO.service[SocketServer]
    x <- s.getHost(clientId)
  } yield x

  def liveConfig:ZLayer[Any, Nothing, SocketServerConfig] = ZLayer.succeed(SocketServerConfig(8092))
  def liveConfigWithPort (port:Int):ZLayer[Any, Nothing, SocketServerConfig] = ZLayer.succeed(SocketServerConfig(port))

  def live:ZLayer[SocketServerConfig, Nothing, SocketServer] = for {
    ref <- ZLayer.fromZIO(TRef.makeCommit[RequestLookup](Map()))
    cfg <- ZLayer.service[SocketServerConfig]
    x <- ZLayer.succeed[SocketServer](SocketServerImpl(ref.get, cfg.get))
  } yield x
}
