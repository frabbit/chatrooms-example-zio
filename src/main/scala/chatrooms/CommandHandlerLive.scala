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

final case class CommandHandlerLive(
  sendDirectMessage:SendDirectMessage,
  join:Join,
) extends CommandHandler {
  def handleCommand(cmd: Command, clientId: ClientId): ZStream[Any, Nothing, WebSocketFrame] =
    cmd.match {
      case Command.JoinRoom(roomName) => for {
        c <- ZStream.succeed(WebSocketFrame.text(ServerMessage.Acknowledge("joinRoom").encode))
      } yield c
      case Command.SendDirectMessage(to, msg) =>
        ZStream.fromZIO(
          sendDirectMessage.sendDirectMessage(clientId, to, msg).map(_.encode).map(WebSocketFrame.text)
        )
      case Command.Join(userName) =>
        ZStream.fromZIO(
          join.join(clientId, userName).map(_.encode).map(WebSocketFrame.text)
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