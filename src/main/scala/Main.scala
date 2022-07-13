import zio._
import chatrooms.socketapp.Server

object MyApp extends ZIOAppDefault {
  def run = Server.run
}