package chatrooms.domain

case class Client(id:ClientId, name:UserName)

case class Room(name:RoomName, clients: Set[ClientId]) {
  def addClient (client:Client) = Room(this.name, this.clients + client.id)
  def removeClient (client:Client) = Room(this.name, this.clients.filter( c => c != client.id))
}

case class ServerState(clients:Map[ClientId, Client], rooms:Map[RoomName, Room]) {
  def addClient (client:Client):ServerState = this.match {
    case ServerState(clients, rooms) => ServerState(clients + (client.id -> client), rooms)
  }

  def getClientIdsOfRoom (roomName:RoomName): Option[Set[ClientId]] =
    this.rooms.get(roomName).map(_.clients.toSet)
  def getRoomNamesOfClient (client:Client): Set[RoomName] =
    this.rooms.keySet

  def removeClient (client:Client):ServerState =
    val roomsOfClient = this.rooms.filter( r => r._2.clients.contains(client.id))
    val cleanRooms = roomsOfClient.values.foldLeft(this)((acc, room) => acc.leaveRoom(room.name, client))
    ServerState(
      this.clients.filter(c => c._1 != client.id),
      cleanRooms.rooms
    )

  def joinRoom (roomName:RoomName, client:Client):ServerState = this.rooms.get(roomName).match {
    case Some(room) =>
      ServerState(clients, rooms + (roomName -> room.addClient(client)))
    case None =>
      ServerState(this.clients, this.rooms + (roomName -> Room(roomName, Set(client.id))))
  }

  def leaveRoom (roomName:RoomName, client:Client):ServerState = this.match {
    case ServerState(clients, rooms) => {
      val newRooms:Map[RoomName, Room] = rooms.get(roomName).match {
        case Some(r) => (rooms + (roomName -> r.removeClient(client))).filter((_, r) => r.clients.size > 0)
        case None => rooms
      }
      ServerState(clients, newRooms)
    }
  }
}

object ServerState {
  def empty () = ServerState(Map(), Map())
}
