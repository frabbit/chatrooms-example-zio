package chatrooms

import chatrooms.usecases.Join
import chatrooms.usecases.SendDirectMessage
import chatrooms.domain.ClientId
import chatrooms.domain.Command
import zhttp.socket.WebSocketFrame
import zio.stream.ZStream
import chatrooms.domain.ServerMessage
import zio.ZIO
import zio.ZLayer
import zio.ZEnvironment
import chatrooms.usecases.JoinRoom
import chatrooms.usecases.ListRoomMembers

final case class CommandHandlerLive(
  sendDirectMessage:SendDirectMessage,
  joinRoom:JoinRoom,
  listRoomMembers: ListRoomMembers,
  join:Join,
) extends CommandHandler {
  def handleCommand(cmd: Command, clientId: ClientId): ZStream[Any, Nothing, WebSocketFrame] =
    cmd.match {
      case Command.JoinRoom(roomName) =>
        ZStream.fromZIO(
          joinRoom.run(clientId, roomName).map(_.encode).map(WebSocketFrame.text)
        )

      case Command.SendDirectMessage(to, msg) =>
        ZStream.fromZIO(
          sendDirectMessage.run(clientId, to, msg).map(_.encode).map(WebSocketFrame.text)
        )
      case Command.Join(userName) =>
        ZStream.fromZIO(
          join.run(clientId, userName).map(_.encode).map(WebSocketFrame.text)
        )
      case Command.ListRoomMembers(roomName) =>
        ZStream.fromZIO(
          listRoomMembers.run(clientId, roomName).map(_.encode).map(WebSocketFrame.text)
        )
      case _ => ZStream.empty
    }
}

case class MapReturn[A]() {
  def apply [R,E,B <: A](layer:ZLayer[R,E,B]):ZLayer[R,E,A] = layer.map((e) => (e : ZEnvironment[A]))
}

def mapReturn[A] = new MapReturn[A]


object CommandHandlerLive {
  def layer = mapReturn[CommandHandler](ZLayer.fromFunction(CommandHandlerLive.apply))

}