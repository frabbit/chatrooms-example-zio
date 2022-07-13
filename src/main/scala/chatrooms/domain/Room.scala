package chatrooms.domain

case class Room(name:RoomName, clients: Set[ClientId]) {
  def addClient (clientId:ClientId) = Room(this.name, this.clients + clientId)
  def removeClient (client:Client) = Room(this.name, this.clients.filter( c => c != client.id))
}