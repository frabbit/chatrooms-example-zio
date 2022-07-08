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

def withServer [R, A, E](run: ZIO[R, E, A]) =
  val acquire =
    for
      s <- Server.app.fork
      _ <- ZIO.sleep(300.milliseconds)
    yield s
  ZIO.acquireReleaseWith(acquire)(_.interruptFork)(_ => run)


def mkTestClient (name:ClientName, queue:MsgQueue) =
  val cb = (msg:ServerMessage) =>
    queue.offer(ServerMessageFor(name, msg)).commit.unit

  TestClient.createTypedClient( cb, CommandEncoder.encode, ServerMessage.parse )

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

def mkTestClientWithDeps(name:String, queue: MsgQueue) =
  mkTestClient(name, queue)

def withOneClient[R,E,A](name:ClientName)(run:(ClientHandle,MsgQueue) => ZIO[R,E,A]) =
  val server =
    withServer {
      for {
        queue <- TQueue.unbounded[ServerMessageFor].commit
        _ <- mkTestClient(name, queue).flatMap {
          case (sendPim, fiberClient) => for {
            _ <- run(ClientHandle(name, sendPim), queue)
            _ <- fiberClient.await
            _ <- ZIO.sleep(200.milliseconds)
          } yield ()
        }
      } yield ()
    }
  server *> assertCompletesZIO

def fullSpec = suite("ChatroomsE2E")(
  test("joining a room should be acknowledged") {
    withOneClient("Tom") { (client, queue) =>
      for {
        _ <- sendAndWait(client.send, queue,
          Some(JoinRoom(RoomName("myRoom"))),
          List(ServerMessageFor(client.name, Acknowledge("joinRoom"))))
        _ <- sendOnly(client.send, None)
      } yield ()
    }
  }
).provide(AsyncHttpClientZioBackend.layer(), ZLayer.succeed(ConsoleLive)) @@ TestAspect.withLiveEnvironment

object ChatroomsE2ESpec extends ZIOSpecDefault {
  override def spec = fullSpec
}
