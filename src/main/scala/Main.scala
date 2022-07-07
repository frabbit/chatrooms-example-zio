import zio._
import zio.Console._
import chatrooms.domain.Http

object MyApp extends ZIOAppDefault {

  def run = myAppLogic0.provide(ZLayer.succeed(Console.ConsoleLive))

  def myAppLogic0: ZIO[Console, Nothing, Unit] = myAppLogic1.catchAll( (_:Throwable) => ZIO.succeed(()) )

  def myAppLogic1: ZIO[Console, Throwable, Unit] = myAppLogic
    //.catchSome((t:Throwable) => ZIO.succeed(()))
    .provideSomeLayer[Console](Http.mock)

  val myAppLogic: ZIO[Http with Console, Throwable, Unit] =
    for {
      s    <- Http.get("hey")
      _ <- Console.printLine("got: " ++ s)
      _    <- Console.printLine("Hello! What is your name?")
      name <- Console.readLine
      _    <- Console.printLine(s"Hello, ${name}, welcome to ZIO!")
    } yield ()

   val myAppLogic5 =
    for {
      _    <- Console.printLine("Hello! What is your name?")
      name <- Console.readLine
      _    <- Console.printLine(s"Hello, ${name}, welcome to ZIO!")
    } yield ()
}