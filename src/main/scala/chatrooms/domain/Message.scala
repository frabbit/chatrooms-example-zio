package chatrooms.domain

import parsley.Parsley, Parsley._
import parsley.combinator.{many, some}
import parsley.character.{char, string, digit, oneOf, noneOf}
import parsley.implicits.character.{charLift, stringLift}

class Message

case class JoinRoom(name:RoomName) extends Message
case class LeaveRoom(name:RoomName) extends Message
case class SendText(name:RoomName, txt:String) extends Message
case class SendPing(name:RoomName) extends Message
case class ListRooms() extends Message
case class ListRoomMembers(name:RoomName) extends Message


object MessageParser {
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

  val joinRoomParser: Parsley[JoinRoom] = for {
    _ <- attempt(string(":joinRoom"))
    _ <- char(' ')
    roomName <- roomNameParser
    x <- Parsley.pure(JoinRoom(roomName))
  } yield x

  val roomNameParser: Parsley[RoomName] = for {
    roomName <- some(noneOf(' ', '\n'))
  } yield RoomName(String.valueOf(roomName))

  val leaveRoomParser: Parsley[LeaveRoom] = for {
    _ <- attempt(string(":leaveRoom"))
    _ <- char(' ')
    roomName <- roomNameParser
    x <- Parsley.pure(LeaveRoom(roomName))
  } yield x

  val parser: Parsley[Message] = leaveRoomParser <|> joinRoomParser <|> listRoomsParser <|> listRoomMembersParser <|> sendPingParser
}

object Message {
  def parse (s:String):Option[Message] = MessageParser.parser.parse(s).toOption
}

