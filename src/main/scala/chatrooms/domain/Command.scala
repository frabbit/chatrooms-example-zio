package chatrooms.domain

import parsley.Parsley, Parsley._
import parsley.combinator.{many, some, eof}
import parsley.character.{char, string, digit, oneOf, noneOf}
import parsley.implicits.character.{charLift, stringLift}

sealed trait Command {
  def encode () = CommandEncoder.encode(this)
}

final case class JoinRoom(name:RoomName) extends Command
final case class LeaveRoom(name:RoomName) extends Command
final case class SendText(name:RoomName, txt:String) extends Command
final case class SendPing(name:RoomName) extends Command
final case class ListRooms() extends Command
final case class ListRoomMembers(name:RoomName) extends Command

object CommandEncoder {
  def encode (c:Command):String = c.match {
    case JoinRoom(name) => ":joinRoom " ++ name.value
    case LeaveRoom(name) => ":leaveRoom " ++ name.value
    case ListRoomMembers(name) => ":listRoomMembers " ++ name.value
    case ListRooms() => ":listRooms"
    case SendText(r, t) => ":sendText " ++ r.value ++ " " ++ t
    case SendPing(r) => ":sendPing " ++ r.value
  }
}


object CommandParser {
  val listRoomMembersParser: Parsley[ListRoomMembers] = for {
    _ <- attempt(string(":listRoomMembers"))
    _ <- char(' ')
    roomName <- roomNameParser
    x <- Parsley.pure(ListRoomMembers(roomName))
  } yield x

  val listRoomsParser: Parsley[ListRooms] = for {
    _ <- attempt(string(":listRooms"))
    x <- Parsley.pure(ListRooms())
  } yield x

  val sendPingParser: Parsley[SendPing] = for {
    _ <- attempt(string(":sendPing"))
    _ <- char(' ')
    roomName <- roomNameParser
    x <- Parsley.pure(SendPing(roomName))
  } yield x

  val sendTextParser: Parsley[SendText] = for {
    _ <- attempt(string(":sendText"))
    _ <- char(' ')
    roomName <- roomNameParser
    _ <- char(' ')
    txt <- some(noneOf(' ', '\n'))
    x <- Parsley.pure(SendText(roomName, txt.mkString))
  } yield x

  val joinRoomParser: Parsley[JoinRoom] = for {
    _ <- attempt(string(":joinRoom"))
    _ <- char(' ')
    roomName <- roomNameParser
    x <- Parsley.pure(JoinRoom(roomName))
  } yield x

  val roomNameParser = RoomName.parser

  val leaveRoomParser: Parsley[LeaveRoom] = for {
    _ <- attempt(string(":leaveRoom"))
    _ <- char(' ')
    roomName <- roomNameParser

    x <- Parsley.pure(LeaveRoom(roomName))
  } yield x

  val parser: Parsley[Command] = for {
    c <- leaveRoomParser <|> joinRoomParser <|> listRoomsParser <|> listRoomMembersParser <|> sendPingParser <|> sendTextParser
    s <- eof
  } yield c
}

object Command {
  def parse (s:String):Option[Command] = CommandParser.parser.parse(s).toOption
}

