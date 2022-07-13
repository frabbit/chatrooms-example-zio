package chatrooms.domain

import zio.test.TestAspect
import zio.ZIO
import zio.test.Assertion._
import zio.test._
import zio.test.{Gen, Sized}
import zio.test.Gen as G
import _root_.chatrooms.all.allSyntaxChatrooms

object ServerStateSpec extends ZIOSpecDefault {

  private def spec_ = suite("ServerState")(
    suite("empty should")(
      test("create an empty ServerState") {
        val expectedState = ServerState(Map(), Map())
        assert(ServerState.empty())(equalTo(expectedState))
      }
    ) +
    suite("addClient should") (
      test("add a client to the state") {
        check(Generators.client) { client =>
          val initialState = ServerState.empty()
          val expectedState = ServerState(Map() + (client.id -> client), Map())
          assert(initialState.addClient(client))(equalTo(Right(expectedState)))
        }
      }
      +
      test("do nothing when client was already added") {
        check(Generators.client) { client =>
          val Right(initialState) = ServerState.empty().addClient(client)
          val expectedState = initialState
          assert(initialState.addClient(client))(equalTo(Left(ServerState.ClientExists)))
        }
      }
    ) +
     suite("getRoomNamesOfClient should") (
      test("return an empty list when client has not joined any rooms") {
        check(Generators.client) { client =>
          val state = ServerState.empty().addClient(client).getOrElse(ServerState.empty())
          assert(state.getRoomNamesOfClient(client))(equalTo(Set()))
        }
      } +
      test("return an list with one room when the client joined one room") {
        check(Generators.client, Generators.roomName) { (client, roomName) =>
          val rooms = Map(roomName -> Room(roomName, Set(client.id)))
          val state = ServerState(
            Map(client.id -> client),
            rooms
          )
          assert(state.getRoomNamesOfClient(client))(equalTo(Set(roomName)))
        }
      } +
      test("return an list with multiple rooms when the client joined multiple room") {
        check(Generators.client, G.vectorOf(Generators.roomName)) { (client, roomNames) =>
          val rooms = Map.from(roomNames.map(n => (n -> Room(n, Set(client.id)))))
          val state = ServerState(
            Map(client.id -> client),
            rooms
          )
          assert(state.getRoomNamesOfClient(client))(equalTo(roomNames.toSet))
        }
      }
    ) +
    suite("getClientIdsOfRoom should") (
      test("return a set of all clients of a room") {
        check(G.listOfBounded(0, 0)(Generators.client), Generators.roomName) { (clients, roomName) =>
          val rooms = Map(roomName -> Room(roomName, clients.map(_.id).toSet))
          val state = ServerState(
            Map.from(clients.map(c => c.id -> c)),
            rooms
          )
          assert(state.getClientIdsOfRoom(roomName))(equalTo(Some(clients.map(_.id).toSet)))
        }
      }
    ) +
    suite("removeClient should") (
      test("remove the client from the state") {
        check(Generators.client) { client =>
          val initialState = ServerState(
            Map(client.id -> client),
            Map.empty
          )
          val expectedState = ServerState(
            Map.empty,
            Map.empty
          )
          assert(initialState.removeClient(client))(equalTo(expectedState))
        }
      } +
      test("remove the client from all rooms") {
        check(Generators.client, Generators.roomName, Generators.roomName) { (client, roomNameA, roomNameB) =>
          val initialState = ServerState(
            Map(client.id -> client),
            Map(
              roomNameA -> Room(roomNameA, Set(client.id)),
              roomNameB -> Room(roomNameB, Set(client.id))
            )
          )
          val expectedState = ServerState(
            Map.empty,
            Map.empty
          )
          assert(initialState.removeClient(client))(equalTo(expectedState))
        }
      }
    ) +
    suite("leaveRoom should") (
      test("remove the client from a room") {
        check(Generators.client, Generators.client, Generators.roomName) { (clientA, clientB, roomName) =>
          val clientMap = Map(clientA.id -> clientA, clientB.id -> clientB)
          val initialState = ServerState(
            clientMap,
            Map(roomName -> Room(roomName, Set(clientA.id, clientB.id)))
          )
          val expectedState = ServerState(
            clientMap,
            Map(roomName -> Room(roomName, Set(clientB.id)))
          )
          assert(initialState.leaveRoom(roomName, clientA))(equalTo(expectedState))
        }
      } +
      test("do nothing if the client is not in the room") {
        check(Generators.client, Generators.client, Generators.roomName) { (clientA, clientB, roomName) =>
          val clientMap = Map(clientA.id -> clientA, clientB.id -> clientB)
          val initialState = ServerState(
            clientMap,
            Map(roomName -> Room(roomName, Set(clientB.id)))
          )
          val expectedState = initialState
          assert(initialState.leaveRoom(roomName, clientA))(equalTo(expectedState))
        }
      }
      +
      test("delete the room if it's empty after leaving") {
        check(Generators.client, Generators.roomName) { (clientA, roomName) =>
          val clientMap = Map(clientA.id -> clientA)
          val initialState = ServerState(
            clientMap,
            Map(roomName -> Room(roomName, Set(clientA.id)))
          )
          val expectedState = ServerState(
            clientMap,
            Map.empty
          )
          assert(initialState.leaveRoom(roomName, clientA))(equalTo(expectedState))
        }
      }

    ) +
    suite("joinRoom should") (
      test("create a new room and add the client to that room when that room does not exist") {
        check(Generators.client, Generators.roomName) { (client, roomName) =>
          val clientMap = Map(client.id -> client)
          val initialState = ServerState(
            clientMap,
            Map()
          )
          val expectedState = ServerState(
            clientMap,
            Map(roomName -> Room(roomName, Set(client.id)))
          )
          assert(initialState.joinRoom(roomName, client.id))(equalTo(expectedState))
        }
      } +
      test("add the client to the room when it exist already") {
        check(Generators.client, Generators.client, Generators.roomName) { (clientA, clientB, roomName) =>
          val clientMap = Map(clientA.id -> clientA, clientB.id -> clientB)
          val initialState = ServerState(
            clientMap,
            Map(roomName -> Room(roomName, Set(clientA.id)))
          )
          val expectedState = ServerState(
            clientMap,
            Map(roomName -> Room(roomName, Set(clientB.id, clientA.id)))
          )
          assert(initialState.joinRoom(roomName, clientB.id))(equalTo(expectedState))
        }
      }
    )
  ) @@ _root_.chatrooms.CustomTestConfig.use


  override def spec = spec_
}