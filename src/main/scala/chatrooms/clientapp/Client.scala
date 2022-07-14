package chatrooms.clientapp

import scala.language.postfixOps
import sttp.client3._
import sttp.client3.asynchttpclient.zio._
import sttp.ws.{WebSocket}
import zio._
import zio.Console._
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import chatrooms.domain.CommandParser
import chatrooms.domain.Command
import scala.io.StdIn
import zio.stm.TRef
import zio.stm.TQueue
import java.io.IOException
import org.jline.terminal.Terminal
import org.jline.terminal.TerminalBuilder
import java.io.PrintWriter
import org.jline.utils.NonBlockingReader
import org.jline.terminal.Attributes.ControlChar
import org.jline.utils.InfoCmp.Capability
import chatrooms.all.allSyntaxChatrooms
import java.util.function.IntConsumer
import org.jline.reader.impl.LineReaderImpl
import org.jline.reader.LineReader
import org.jline.reader.LineReaderBuilder
import org.asynchttpclient.ClientStats

trait Input:
  def toList:List[Int]

object Input:
  final case class Code(code:Int) extends Input:
    def toList = List(code)

  final case class EscapeSequence(chars:List[Int]) extends Input:
    def toList = chars

case class State(input:String, messages:List[String], rawInput:List[List[Int]])

object State {
  def initial = State("", List("successfully connected to server...", "type \"help\" for usage information"), List.empty)
}

case class TerminalHandle(lineReader:LineReader, writer:PrintWriter, reader:NonBlockingReader, terminal:Terminal) {

  def readCharacter = ZIO.attempt {
    lineReader.asInstanceOf[LineReaderImpl].readCharacter()
  }
  def readInt = ZIO.attempt {
    reader.read()
  }
  def printLine (s:String) = print(s + "\n")

  def flush () = ZIO.attempt{
    writer.flush()
    ()
  }
  def print(s:String) = ZIO.attempt {
    writer.print(s)
    ()
  }
  def write(s:String) = ZIO.attempt {
    writer.write(s)
    ()
  }
  def clear = ZIO.attempt {
    terminal.puts(Capability.clear_screen)
    terminal.puts(Capability.cursor_home)
    terminal.flush()
    ()
  }
}

object TerminalUtils {
  def withTerminal =
    val cleanup = (t:TerminalHandle) => ZIO.attempt {
      t.reader.close()
      t.writer.close()
      t.terminal.close()
      ()
    }.ignore
    ZIO.acquireReleaseWith(ZIO.attempt {
    val terminal:Terminal = TerminalBuilder.builder()
      .jna(true)
      .system(true)

      .build();
    terminal.enterRawMode()
    val writer = terminal.writer()
    val reader = terminal.reader()
    val lineReader = LineReaderBuilder.builder()
                    .terminal(terminal)
                    .build
    TerminalHandle(lineReader, writer, reader, terminal)
  })(cleanup)
}

object TerminalInput {

}

object KeyCode {
  val BACKSPACE = 127
  val LEFT = 37
  val RIGHT = 39
  val UP = 38
  val DOWN = 40
  val ESCAPE = 27
  val ENTER = 13
}

type RedrawQueue = TQueue[Unit]

type ClientStateRef = TRef[State]

object Client extends ZIOAppDefault {

  def useWebSocket(ws: WebSocket[[X] =>> ZIO[Console, Throwable, X]]): RIO[Console, Unit] =
    def inputLoop(state:ClientStateRef, queue:RedrawQueue, t:TerminalHandle):ZIO[Console, Throwable, Unit] =
      def applyInput (inputLine:String, input:Input):(Option[String], String) = {
        (inputLine.length, input).match {
          case (0, Input.Code(KeyCode.BACKSPACE)) => (None, "")
          case (l, Input.Code(KeyCode.BACKSPACE)) => (None, inputLine.substring(0, l - 1))
          case (_, Input.EscapeSequence(_)) => (None, inputLine)
          case (0, Input.Code(KeyCode.ENTER)) => (None, "")
          case (_, Input.Code(KeyCode.ENTER)) => (Some(inputLine), "")
          case (_, Input.Code(c)) => (None, inputLine + c.toChar.toString())
        }
      }

      def readEscapeSequence = for {
        sec <- t.readCharacter
        third <- if sec == '[' then t.readCharacter.map(Some(_)) else ZIO.succeed(None)
        result <- ZIO.succeed(Input.EscapeSequence(List(KeyCode.ESCAPE, sec) ++ third.toList))
      } yield result

      def processLine (line:String):RIO[Console,Unit] =
        Command.parse(line).match {
          case Some(_) => ws.sendText(line)
          case None => line.match {
            case "exit" => exit(ExitCode(0))
            case "help" =>
              val helpLines = List(
                "Available commands:",
                ":join <userName>",
                ":sendDirectMessage <userName> <message>",
                ":listRooms",
                ":joinRoom <roomName>",
                ":sendMessageToRoom <roomName> <message>",
                ":listMembers <roomName>"
              )
              (state.update(s => s.copy(messages = s.messages ++ helpLines)) *> requestRedraw(queue)).commit *> ZIO.unit
            case _ => (state.update(s => s.copy(messages = s.messages :+ ("Cannot parse command: " ++ line))) *> requestRedraw(queue)).commit *> ZIO.unit
          }
        }

      def readInput =
        for
          code <- t.readCharacter
          input <- if code == KeyCode.ESCAPE then readEscapeSequence else ZIO.succeed(Input.Code(code))
        yield input
      for
        input <- readInput
        update <- state.modify(s => {
          val (newLine, newInput) = applyInput(s.input, input)
          val newState = State(newInput, s.messages, s.rawInput :+ input.toList )
          val hasChanged = s.input != newState.input || s.messages != newState.messages
          ((newLine, hasChanged), newState)
        }).commit
        (nextLine, hasChanged) = update
        _ <- nextLine.map(processLine).getOrElse(ZIO.unit)
        _ <- ZIO.when(hasChanged)(requestRedraw(queue).commit)
        _ <- inputLoop(state, queue, t)
      yield ()

    def requestRedraw (queue:RedrawQueue) = queue.offer(())

    def receiveLoop(state:ClientStateRef, update:RedrawQueue, t:TerminalHandle):RIO[Console,Unit] =
      for
        msg <- ws.receiveText()
        _ <- state.update(s => State(s.input, s.messages :+ "Received: " + msg, s.rawInput)).commit
        _ <- requestRedraw(update).commit
        _ <- receiveLoop(state, update, t)
      yield ()

    def drawLoop(state:ClientStateRef, update:RedrawQueue, t:TerminalHandle):Task[Unit] =
      for
        _ <- update.take.commit
        _ <- t.clear
        h <- ZIO.attempt { t.terminal.getHeight() }
        w <- ZIO.attempt { t.terminal.getWidth() }
        s <- state.get.commit
        missing = Math.max((h-2) - s.messages.length, 0)
        allMessages = s.messages ++ List.fill(missing)("")
        _ <- ZIO.collectAllDiscard(allMessages.takeRight(h-2).map(m => t.printLine(m)))
        _ <- t.printLine(List.fill(w)("=").mkString)
        _ <- t.print(s.input)
        _ <- t.flush()
        _ <- drawLoop(state, update, t)
      yield ()

    for {
      state <- TRef.make(State.initial).commit
      queue <- TQueue.dropping[Unit](1).commit
      _ <- TerminalUtils.withTerminal { t =>
        t.clear *> requestRedraw(queue).commit *>
        ZIO.collectAllParDiscard(List(
          receiveLoop(state, queue, t),
          inputLoop(state, queue, t).forkDaemon,
          drawLoop(state, queue, t)
        ))
      }
    } yield ()

  // create a description of a program, which requires two dependencies in the environment:
  // the SttpClient, and the Console
  val sendAndPrint: RIO[Console with SttpClient, Response[Unit]] =
    sendR(basicRequest.get(uri"ws://127.0.0.1:8092").response(asWebSocketAlways(useWebSocket)))

  override def run =
    Console.printLine("Client starting...")
    *> sendAndPrint.provide(AsyncHttpClientZioBackend.layer(), ZLayer.succeed(ConsoleLive)).catchAll { t =>
      Console.printLine(s"Cannot connect to server... $t")
    }
    |> (_.exitCode)
}
