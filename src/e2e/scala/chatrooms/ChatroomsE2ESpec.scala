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

type ClientName = String

case class WaitError(msg:List[ServerMessageFor])

case class ServerMessageFor(name:ClientName,msg:ServerMessage)

type MsgQueue = TQueue[ServerMessageFor]

case class ClientHandle (name:ClientName, send: Send[Command])

def fullSpec = suite("ChatroomsE2E")(
  test("joining the server should be acknowledged") {
    withOneClient("Tom") { (client, queue) =>
      for {
        _ <- Api.join(client, queue)
        _ <- Api.exitClient(client)
      } yield ()
    }
  } +
  test("sending a direct message between clients should work as expected") {
    withTwoClients("Tom", "Abe") { (clientA, clientB, queue) =>
      for {
        _ <- Api.join(clientA, queue)
        _ <- Api.join(clientB, queue)
        _ <- Api.sendAndReceiveDirectMessage(clientA, clientB, queue, "a message from A to B")
        _ <- Api.exitClient(clientA)
        _ <- Api.exitClient(clientB)
      } yield ()
    }
  }
  +
  test("listRooms should list all available rooms on the server") {
    withOneClient("Tom") { (client, queue) =>
      val roomA = RoomName("roomA")
      val roomB = RoomName("roomB")
      for {
        _ <- Api.join(client, queue)
        _ <- Api.joinRoom(client, queue, roomA)
        _ <- Api.joinRoom(client, queue, roomB)
        _ <- Api.listRoomsShouldMatch(client, queue, Set(roomA, roomB))
        _ <- Api.exitClient(client)
      } yield ()
    }
  }
  +
  test("joining a room should be acknowledged") {
    withOneClient("Tom") { (client, queue) =>
      for {
        _ <- Api.join(client, queue)
        _ <- Api.joinRoom(client, queue, RoomName("myRoom"))
        _ <- Api.exitClient(client)
      } yield ()
    }
  }
  +
  test("listRoomMembers should list all members of that room") {
    withTwoClients("Tom", "Jack") { (clientA, clientB, queue) =>
      for {
        _ <- Api.join(clientA, queue)
        _ <- Api.join(clientB, queue)
        roomName = RoomName("myRoom")
        _ <- Api.joinRoom(clientA, queue, roomName)
        _ <- Api.joinRoom(clientB, queue, roomName)
        expectedMembers = Set(UserName(clientA.name), UserName(clientB.name))
        _ <- Api.listRoomMembersShouldMatch(clientA, queue, roomName, expectedMembers)
        _ <- Api.listRoomMembersShouldMatch(clientB, queue, roomName, expectedMembers)
        _ <- Api.exitClient(clientA)
        _ <- Api.exitClient(clientB)
      } yield ()
    }
  }
  +
  test("sendMessageToRoom should send the message to all members of a room") {
    withFourClients("Tom", "Jack", "Sarah", "Jane") { (clientA, clientB, clientC, clientD, queue) =>
      val all = List(clientA, clientB, clientC, clientD)
      for {
        _ <- Api.joinAll(all, queue)
        roomName = RoomName("myRoom")
        _ <- Api.joinRoomAll(all, queue, roomName)
        _ <- Api.sendMessageToRoom(clientA, all, queue, roomName, "hello world")
        _ <- Api.exitAll(all)
      } yield ()
    }
  }
).provide(AsyncHttpClientZioBackend.layer(), ZLayer.succeed(ConsoleLive)) @@ TestAspect.withLiveEnvironment @@ TestAspect.sequential


object ChatroomsE2ESpec extends ZIOSpecDefault {
  override def spec = fullSpec
}

object Api {
  def joinAll (clients:List[ClientHandle], queue:MsgQueue) =
    ZIO.collectAll(clients.map(Api.join(_, queue))) *> ZIO.unit

  def exitAll (clients:List[ClientHandle]) =
    ZIO.collectAll(clients.map(Api.exitClient(_))) *> ZIO.unit

  def joinRoomAll (clients:List[ClientHandle], queue:MsgQueue, roomName:RoomName) =
    ZIO.collectAll(clients.map(Api.joinRoom(_, queue, roomName))) *> ZIO.unit

  def join (c:ClientHandle, queue:MsgQueue) =
    sendAndWait(c.send, queue,
          Some(Command.Join(UserName(c.name))),
          List(ServerMessageFor(c.name, ServerMessage.Acknowledge("join"))))

  def sendAndReceiveDirectMessage (sender:ClientHandle, receiver:ClientHandle, queue:MsgQueue, msg:String) =
    sendAndWait(sender.send, queue,
      Some(Command.SendDirectMessage(UserName(receiver.name), msg)),
      List(
        ServerMessageFor(sender.name, ServerMessage.Acknowledge("sendDirectMessage")),
        ServerMessageFor(receiver.name, ServerMessage.DirectMessage(UserName(sender.name), msg ))
      )
    )
  def sendMessageToRoom (sender:ClientHandle, receivers:List[ClientHandle], queue:MsgQueue, roomName:RoomName, msg:String) =
    sendAndWait(sender.send, queue,
      Some(Command.SendMessageToRoom(roomName, msg)),
      receivers.map(r => ServerMessageFor(r.name, ServerMessage.RoomMessage(UserName(sender.name), roomName, msg )))
    )
  def joinRoom (c:ClientHandle, queue: MsgQueue, roomName:RoomName) =
    sendAndWait(c.send, queue,
          Some(Command.JoinRoom(roomName)),
          List(ServerMessageFor(c.name, ServerMessage.Acknowledge("joinRoom"))))

  def listRoomsShouldMatch(c:ClientHandle, queue:MsgQueue, rooms:Set[RoomName]) =
    sendAndWait(c.send, queue,
          Some(Command.ListRooms),
          List(ServerMessageFor(c.name, ServerMessage.AllRoomNames(rooms))))
  def listRoomMembersShouldMatch (c:ClientHandle, queue: MsgQueue, roomName:RoomName, expectedMembers:Set[UserName]) =
    sendAndWait(
          c.send, queue,
          Some(Command.ListRoomMembers(roomName)),
          List(ServerMessageFor(c.name, ServerMessage.AllRoomMembers(roomName, expectedMembers)))
          )

  def exitClient (c:ClientHandle) =
    sendOnly(c.send, None)
}