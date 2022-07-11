package chatrooms

import chatrooms.domain.Command
import chatrooms.domain.ClientId
import zio.stream.ZStream
import zhttp.socket.WebSocketFrame

trait CommandHandler {
  def handleCommand(cmd: Command, clientId: ClientId): ZStream[Any, Nothing, WebSocketFrame]
}

object CommandHandler {
  def handleCommand(cmd: Command, clientId: ClientId): ZStream[CommandHandler, Nothing, WebSocketFrame] =
    ZStream.serviceWithStream[CommandHandler](_.handleCommand(cmd, clientId))
}
