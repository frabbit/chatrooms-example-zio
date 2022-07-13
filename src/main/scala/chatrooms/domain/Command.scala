package chatrooms.domain

import parsley.Parsley, Parsley._
import parsley.combinator.{many, some, eof}
import parsley.character.{char, string, digit, oneOf, noneOf}
import parsley.implicits.character.{charLift, stringLift}

sealed trait Command {
  def encode = CommandEncoder.encode(this)
}

object CommandEncoder {
  def encode (c:Command):String = c.match {
    case Command.JoinRoom(name) => ":joinRoom " ++ name.encode
    case Command.SendMessageToRoom(roomName, txt) => ":sendMessageToRoom " ++ roomName.encode ++ " " ++ txt
    case Command.LeaveRoom(name) => ":leaveRoom " ++ name.encode
    case Command.ListRoomMembers(name) => ":listRoomMembers " ++ name.encode
    case Command.ListRooms => ":listRooms"
    case Command.SendText(r, t) => ":sendText " ++ r.encode ++ " " ++ t
    case Command.Join(name) => ":join " ++ name.encode
    case Command.SendPing(r) => ":sendPing " ++ r.encode
    case Command.SendDirectMessage(to, txt) => ":sendDirectMessage " ++ to.encode ++ " " ++ txt
  }
}

object CommandParser {
  val listRoomMembersParser: Parsley[Command.ListRoomMembers] = for {
    _ <- attempt(string(":listRoomMembers"))
    _ <- char(' ')
    roomName <- RoomName.parser
    x <- Parsley.pure(Command.ListRoomMembers(roomName))
  } yield x

  val listRoomsParser: Parsley[Command.ListRooms] = for {
    _ <- attempt(string(":listRooms"))
    x <- Parsley.pure(Command.ListRooms)
  } yield x

  val joinParser: Parsley[Command.Join] = for {
    _ <- attempt(string(":join"))
    _ <- char(' ')
    userName <- UserName.parser
    x <- Parsley.pure(Command.Join(userName))
  } yield x

  val sendDirectMessageParser: Parsley[Command.SendDirectMessage] = for {
    _ <- attempt(string(":sendDirectMessage"))
    _ <- char(' ')
    userName <- UserName.parser
    _ <- char(' ')
    msg <- many(noneOf('\n'))
    x <- Parsley.pure(Command.SendDirectMessage(userName, msg.mkString))
  } yield x

  val sendMessageToRoomParser: Parsley[Command.SendMessageToRoom] = for {
    _ <- attempt(string(":sendMessageToRoom"))
    _ <- char(' ')
    roomName <- RoomName.parser
    _ <- char(' ')
    msg <- many(noneOf('\n'))
    x <- Parsley.pure(Command.SendMessageToRoom(roomName, msg.mkString))
  } yield x

  val sendPingParser: Parsley[Command.SendPing] = for {
    _ <- attempt(string(":sendPing"))
    _ <- char(' ')
    roomName <- RoomName.parser
    x <- Parsley.pure(Command.SendPing(roomName))
  } yield x

  val sendTextParser: Parsley[Command.SendText] = for {
    _ <- attempt(string(":sendText"))
    _ <- char(' ')
    roomName <- RoomName.parser
    _ <- char(' ')
    txt <- some(noneOf(' ', '\n'))
    x <- Parsley.pure(Command.SendText(roomName, txt.mkString))
  } yield x

  val joinRoomParser: Parsley[Command.JoinRoom] = for {
    _ <- attempt(string(":joinRoom"))
    _ <- char(' ')
    roomName <- RoomName.parser
    x <- Parsley.pure(Command.JoinRoom(roomName))
  } yield x

  val leaveRoomParser: Parsley[Command.LeaveRoom] = for {
    _ <- attempt(string(":leaveRoom"))
    _ <- char(' ')
    roomName <- RoomName.parser
    x <- Parsley.pure(Command.LeaveRoom(roomName))
  } yield x

  val parser: Parsley[Command] = for {
    c <-     leaveRoomParser
         <|> joinRoomParser
         <|> listRoomsParser
         <|> listRoomMembersParser
         <|> sendPingParser
         <|> sendTextParser
         <|> joinParser
         <|> sendDirectMessageParser
         <|> sendMessageToRoomParser
    s <- eof
  } yield c
}

object Command {
  def parse (s:String):Option[Command] = CommandParser.parser.parse(s).toOption

  final case class SendMessageToRoom(roomName:RoomName, txt:String) extends Command
  final case class JoinRoom(name:RoomName) extends Command
  final case class LeaveRoom(name:RoomName) extends Command
  final case class SendText(name:RoomName, txt:String) extends Command
  final case class SendPing(name:RoomName) extends Command
  case object ListRooms extends Command
  type ListRooms = ListRooms.type
  final case class ListRoomMembers(name:RoomName) extends Command
  final case class Join(name:UserName) extends Command
  final case class SendDirectMessage(to:UserName, txt:String) extends Command
}

