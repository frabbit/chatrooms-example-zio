package chatrooms.domain

import zio.test.TestAspect
import zio.ZIO
import zio.{UIO}

import zio.test.Assertion._
import zio.test._
import zio.test.{Gen, Sized}
import zio.test.Gen as G
import zio.Random
import _root_.chatrooms.all.allSyntaxChatrooms
import _root_.chatrooms.Server
import _root_.chatrooms.ServerConfig
import _root_.chatrooms.TestClient
import _root_.chatrooms.Send
import zio.stm.TQueue
import zio.durationInt
import zio.Clock
import zio.ZEnvironment
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.Console.ConsoleLive
import zio.ZLayer


type ClientName = String

case class WaitError(msg:List[ServerMessageFor])

case class ServerMessageFor(name:ClientName,msg:ServerMessage)

type MsgQueue = TQueue[ServerMessageFor]

case class ClientHandle (name:ClientName, send: Send[Command])

def withServer [R, A, E](run: ServerConfig => ZIO[R, E, A]) = for {
  port <- Random.nextIntBetween(1024, 60000)
  cfg = ServerConfig(port)
  acquire =
    for
      s <- Server.app(cfg).fork
      _ <- ZIO.sleep(300.milliseconds)
    yield s
  r <- ZIO.acquireReleaseWith(acquire)(_.interrupt.forkDaemon)(_ => run(cfg))
  } yield r


def mkTestClient (name:ClientName, queue:MsgQueue, cfg:ServerConfig) =
  val cb = (msg:ServerMessage) =>
    queue.offer(ServerMessageFor(name, msg)).commit.unit

  TestClient.createTypedClient( cb, CommandEncoder.encode, ServerMessage.parse, cfg )

def waitForAll (queue: MsgQueue, msgs: List[ServerMessageFor]):UIO[Unit] =
  def go (left:List[ServerMessageFor]):UIO[Unit] =
    if left.length == 0
    then ZIO.unit
    else queue.take.commit.flatMap(msg => go(msgs.filter(_ != msg)))
  go(msgs)

def sendAndWait (send:Send[Command], queue: MsgQueue, cmd: Option[Command], expectedMessages:List[ServerMessageFor]) =
  for {
    _ <- send(cmd)
    res <- waitForAll(queue, expectedMessages).timeout(400.milliseconds)
    _ <- if res.isEmpty then ZIO.fail(WaitError(expectedMessages)) else ZIO.unit
  } yield ()

def sendOnly (send:Send[Command], cmd: Option[Command]) = send(cmd).zip(ZIO.sleep(400.milliseconds))

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
            _ <- ZIO.sleep(200.milliseconds)
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
        _ <- ZIO.sleep(200.milliseconds)


      } yield ()
    }
  server *> assertCompletesZIO

def fullSpec = suite("ChatroomsE2E")(
  test("joining should be acknowledged") {
    withOneClient("Tom") { (client, queue) =>
      for {
        _ <- sendAndWait(client.send, queue,
          Some(Join(UserName("Pim"))),
          List(ServerMessageFor(client.name, Acknowledge("join"))))
        _ <- sendOnly(client.send, None)
      } yield ()
    }
  }
  +
  test("joining should only work once") {
    withOneClient("Tom") { (client, queue) =>
      for {
        _ <- sendAndWait(client.send, queue,
          Some(Join(UserName(client.name))),
          List(ServerMessageFor(client.name, Acknowledge("join"))))
        _ <- sendAndWait(client.send, queue,
          Some(Join(UserName(client.name))),
          List(ServerMessageFor(client.name, SMError(SEAlreadyJoined()))))

        _ <- sendOnly(client.send, None)
      } yield ()
    }
  }
  +
  test("Joining of a second user joining with the same name should fail") {
    withTwoClients("Tom", "Abe") { (clientA, clientB, queue) =>
      for {
        _ <- sendAndWait(clientA.send, queue,
          Some(Join(UserName(clientA.name))),
          List(ServerMessageFor(clientA.name, Acknowledge("join"))))
        _ <- sendAndWait(clientB.send, queue,
          Some(Join(UserName(clientA.name))),
          List(ServerMessageFor(clientB.name, SMError(SEAlreadyJoined()))))


        _ <- sendOnly(clientA.send, None)
        _ <- sendOnly(clientB.send, None)
      } yield ()
    }
  }
).provide(AsyncHttpClientZioBackend.layer(), ZLayer.succeed(ConsoleLive)) @@ TestAspect.withLiveEnvironment @@ TestAspect.sequential

object ChatroomsE2ESpec extends ZIOSpecDefault {
  override def spec = fullSpec
}
