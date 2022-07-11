import zio._
import zio.Console._
import zhttp.service.Server.apply
import chatrooms.Server

object MyApp extends ZIOAppDefault {

  def run = Server.run

}