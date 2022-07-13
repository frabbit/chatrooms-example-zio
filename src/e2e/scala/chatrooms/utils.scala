package chatrooms

import zio.test.TestAspect
import zio.ZIO
import zio.{UIO}

import zio.test.Assertion._
import zio.test._
import zio.test.{Gen, Sized}
import zio.test.Gen as G
import zio.Random
import chatrooms.all.allSyntaxChatrooms
import chatrooms.socketapp.Server
import chatrooms.socketapp.ServerConfig
import chatrooms.domain.ServerMessage
import chatrooms.domain.ServerError
import chatrooms.domain.Command
import chatrooms.domain.CommandEncoder
import chatrooms.domain.UserName
import chatrooms.domain.RoomName

import _root_.chatrooms.clientapp.{TestClient, Send}
import zio.stm.TQueue
import zio.durationInt
import zio.Clock
import zio.ZEnvironment
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.Console.ConsoleLive
import zio.Console
import zio.ZLayer
import chatrooms.utils.Ports
import chatrooms.domain

def findPort:ZIO[Any, Nothing, Int] = for {
  port <- Random.nextIntBetween(Ports.MIN_PORT_NUMBER, Ports.MAX_PORT_NUMBER + 1)
  avail <- Ports.available(port)
  _ <- ZIO.when(!avail)(zio.Console.printLine("port " ++ port.toString() ++ " not available").ignore)
  x <- if avail then ZIO.succeed(port) else findPort
} yield x



def withServer [R, A, E](run: ServerConfig => ZIO[R, E, A]) =
  def waitUntilServerIsAvailable (port:Int):UIO[Unit] =
    Ports.available(port).flatMap(b => if b then ZIO.sleep(50.milliseconds) *> waitUntilServerIsAvailable(port) else ZIO.unit)
  for {
  port <- findPort
  cfg = ServerConfig(port)

  acquire =
    for
      s <- Server.app(cfg).fork
      _ <- waitUntilServerIsAvailable(port)

    yield s
  r <- ZIO.acquireReleaseWith(acquire)(_.interrupt.forkDaemon)(_ => run(cfg))
  } yield r


def mkTestClient (name:ClientName, queue:MsgQueue, cfg:ServerConfig) =
  val cb = (msg:ServerMessage) =>
    queue.offer(ServerMessageFor(name, msg)).commit.unit
  TestClient.createTypedClient( cb, CommandEncoder.encode, ServerMessage.parse, TestClient.Config(cfg.port) )

def waitForAll (queue: MsgQueue, msgs: List[ServerMessageFor]):UIO[Unit] =
  def go (left:List[ServerMessageFor]):UIO[Unit] =
    if left.length == 0
    then ZIO.unit
    else queue.take.commit.flatMap(msg =>
      go(left.filter(_ != msg))
    )
  go(msgs)

def sendAndWait (send:Send[Command], queue: MsgQueue, cmd: Option[Command], expectedMessages:List[ServerMessageFor]) =
  for {
    _ <- send(cmd)
    res <- waitForAll(queue, expectedMessages).timeout(450.milliseconds)
    _ <- if res.isEmpty then ZIO.fail(WaitError(expectedMessages)) else ZIO.unit
  } yield ()

def sendOnly (send:Send[Command], cmd: Option[Command]) = send(cmd).zip(ZIO.sleep(250.milliseconds))

def mkTestClientWithDeps(name:String, queue: MsgQueue, cfg:ServerConfig) =
  mkTestClient(name, queue, cfg)

def withOneClient[R,E,A](name:ClientName)(run:(ClientHandle,MsgQueue) => ZIO[R,E,A]) =
  val server =
    withServer { cfg =>
      for {
        queue <- TQueue.unbounded[ServerMessageFor].commit
        _ <- mkTestClient(name, queue, cfg).flatMap {
          case (sendPim, fiberClient) => for {
            _ <- run(ClientHandle(name, sendPim), queue)
            _ <- fiberClient.await
            _ <- ZIO.sleep(400.milliseconds)
          } yield ()
        }
      } yield ()
    }
  server *> assertCompletesZIO

def withTwoClients[R,E,A](nameA:ClientName, nameB:ClientName)(run:(ClientHandle,ClientHandle,MsgQueue) => ZIO[R,E,A]) =
  val server =
    withServer { cfg =>
      for {
        queue <- TQueue.unbounded[ServerMessageFor].commit
        a <- mkTestClient(nameA, queue, cfg)
        b <- mkTestClient(nameB, queue, cfg)
        _ <- run(ClientHandle(nameA, a._1), ClientHandle(nameB, b._1), queue)
        _ <- a._2.await
        _ <- b._2.await
        _ <- ZIO.sleep(400.milliseconds)


      } yield ()
    }
  server *> assertCompletesZIO

def withFourClients[R,E,A](nameA:ClientName, nameB:ClientName, nameC:ClientName, nameD:ClientName)(run:(ClientHandle, ClientHandle, ClientHandle, ClientHandle, MsgQueue) => ZIO[R,E,A]) =
  val server =
    withServer { cfg =>
      for {
        queue <- TQueue.unbounded[ServerMessageFor].commit
        a <- mkTestClient(nameA, queue, cfg)
        b <- mkTestClient(nameB, queue, cfg)
        c <- mkTestClient(nameC, queue, cfg)
        d <- mkTestClient(nameD, queue, cfg)
        _ <- run(
          ClientHandle(nameA, a._1),
          ClientHandle(nameB, b._1),
          ClientHandle(nameC, c._1),
          ClientHandle(nameD, d._1),
          queue)
        _ <- a._2.await
        _ <- b._2.await
        _ <- c._2.await
        _ <- d._2.await
        _ <- ZIO.sleep(400.milliseconds)
      } yield ()
    }
  server *> assertCompletesZIO
