package chatrooms.domain

import java.util.UUID
import parsley.Parsley, Parsley._
import parsley.combinator as C
import parsley.combinator.{many, some, eof, option}
import parsley.character.{char, string, digit, oneOf, noneOf}
import parsley.character as Character
import parsley.implicits.character.{charLift, stringLift}

sealed trait ServerError {
  def encode = ServerErrorEncoder.encode(this)
}

object ServerErrorEncoder {
  def encode (c:ServerError):String = c.match {
    case ServerError.NotJoined => "notJoined"
    case ServerError.UserNameNotFound => "userNameNotFound"
    case ServerError.UserIsNotInRoom => "userIsNotInRoom"
    case ServerError.AlreadyJoined => "alreadyJoined"
    case ServerError.UserNameTaken => "userNameTaken"
    case ServerError.RoomNotFound(room) => "roomNotFound " ++ room.encode
  }
}

object ServerErrorParser {
  val alreadyJoinedParser: Parsley[ServerError.AlreadyJoined] = for {
    _ <- attempt(string("alreadyJoined"))
    c <- Parsley.pure(ServerError.AlreadyJoined)
  } yield c

  val notJoinedParser: Parsley[ServerError.NotJoined] = for {
    _ <- attempt(string("notJoined"))
    c <- Parsley.pure(ServerError.NotJoined)
  } yield c

  val userNameNotFoundParser: Parsley[ServerError.UserNameNotFound] = for {
    _ <- attempt(string("userNameNotFound"))
    c <- Parsley.pure(ServerError.UserNameNotFound)
  } yield c

  val userNameTakenParser: Parsley[ServerError.UserNameTaken] = for {
    _ <- attempt(string("userNameTaken"))
    c <- Parsley.pure(ServerError.UserNameTaken)
  } yield c

  val userIsNotInRoomParser: Parsley[ServerError.UserIsNotInRoom] = for {
    _ <- attempt(string("userIsNotInRoom"))
    c <- Parsley.pure(ServerError.UserIsNotInRoom)
  } yield c

  val roomNotFoundParser: Parsley[ServerError.RoomNotFound] = for {
    _ <- attempt(string("roomNotFound"))
    _ <- char(' ')
    roomName <- RoomName.parser
    c <- Parsley.pure(ServerError.RoomNotFound(roomName))
  } yield c

  val parser: Parsley[ServerError] = for {
    c <-  alreadyJoinedParser
      <|> userNameTakenParser
      <|> roomNotFoundParser
      <|> userIsNotInRoomParser
      <|> notJoinedParser
      <|> userNameNotFoundParser
    _ <- eof
  } yield c
}

object ServerError {
  case object AlreadyJoined extends ServerError
  type AlreadyJoined = AlreadyJoined.type

  case object UserNameTaken extends ServerError
  type UserNameTaken = UserNameTaken.type

  case class RoomNotFound(room:RoomName) extends ServerError

  case object UserIsNotInRoom extends ServerError
  type UserIsNotInRoom = UserIsNotInRoom.type

  case object NotJoined extends ServerError
  type NotJoined = NotJoined.type

  case object UserNameNotFound extends ServerError
  type UserNameNotFound = UserNameNotFound.type
}
