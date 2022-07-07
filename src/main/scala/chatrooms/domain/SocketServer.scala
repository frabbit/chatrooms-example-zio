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


case class SocketServerConfig(port: Int)

type CallbackE[E] = PartialFunction[(ClientId, WebSocketFrame), ZStream[Any & E, Nothing, WebSocketFrame]]

type Callback = CallbackE[Any]

type Start[E] = (onConnect:Callback) => ZIO[Any & E, Nothing, Unit]

trait SocketServer {
  def start:Start[Any]
  def getHost (clientId:ClientId):ZIO[Any, Nothing, String]
}

type RequestLookup = Map[String, Request]

class SocketServerImpl (lookupRef: TRef[RequestLookup], cfg:SocketServerConfig) extends SocketServer {

  private def socket (id: String, callback : Callback) =
    Socket.collect[WebSocketFrame](x => callback((ClientId(id), x)))

  private def app (callback: Callback) =
    Http.collectZIO[Request] {
      case r@(Method.GET -> Path(Vector(), true)) => for {
          uuid <- ZIO.succeed(UUID.randomUUID())
          _ <- lookupRef.update(x => x + (uuid.toString -> r) ).commit
          x <- socket(uuid.toString, callback).toResponse
        } yield x
      case r => for {
        _ <- printLine("wrong" ++ " : " ++ r.method.toString ++ " : " ++ r.path.toList)
        x <- ZIO.succeed(Response.ok)
      } yield x
    }

  def start:Start[Any] = (onConnect:Callback) => for {
    _ <- Server.start(cfg.port, app(onConnect)).exitCode
  } yield ()

  def getHost (clientId:ClientId) = for {
    req <- lookupRef.get.commit.map( _.get(clientId.value))
    _ <- printLine(req.map(_.toString).toString).either
    host <- req.flatMap(_.host).map(_.toString).getOrElse("<nohost>") |> ZIO.succeed
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

  def live:ZLayer[SocketServerConfig, Nothing, SocketServer] = for {
    ref <- ZLayer.fromZIO(TRef.makeCommit[RequestLookup](Map()))
    cfg <- ZLayer.service[SocketServerConfig]
    x <- ZLayer.succeed[SocketServer](SocketServerImpl(ref.get, cfg.get))
  } yield x
}
