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

object SocketServerConfig {
  def defaultLayer:ZLayer[Any, Nothing, SocketServerConfig] = ZLayer.succeed(SocketServerConfig(8092))
  def withPortLayer (port:Int):ZLayer[Any, Nothing, SocketServerConfig] = ZLayer.succeed(SocketServerConfig(port))
}
