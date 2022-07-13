package chatrooms.domain

import java.util.UUID
import parsley.Parsley, Parsley._
import parsley.combinator as C
import parsley.combinator.{many, some, eof, option}
import parsley.character.{char, string, digit, oneOf, noneOf}
import parsley.character as Character
import parsley.implicits.character.{charLift, stringLift}

sealed trait ServerMessage {
  def encode = ServerMessageEncoder.encode(this)
}

object ServerMessageEncoder {
  def encode (c:ServerMessage):String = c.match {
    case ServerMessage.RoomMessage(from, roomName, txt) => ":roomMessage " ++ from.value ++ " " ++ roomName.value ++ " " ++ txt
    case ServerMessage.Error(error) => ":error " ++ error.encode
    case ServerMessage.Acknowledge(name) => ":acknowledge " ++ name
    case ServerMessage.AllRoomNames(names) => ":allRoomNames " ++ names.mkString(",")
    case ServerMessage.AllRoomMembers(roomName:RoomName, members: Set[UserName]) => ":allRoomMembers " ++ roomName.value ++ " " ++ members.mkString(",")
    case ServerMessage.DirectMessage(from, txt) => ":directMessage " ++ from.value ++ " " ++ txt
  }
}

object ServerMessageParser {
  val roomNameParser = RoomName.parser

  val acknowledgeParser: Parsley[ServerMessage.Acknowledge] = for {
    _ <- attempt(string(":acknowledge"))
    _ <- char(' ')
    txt <- some(noneOf(' ', '\n'))
    x <- Parsley.pure(ServerMessage.Acknowledge(txt.mkString))
  } yield x

  def listOfParser [A](sub:Parsley[A]): Parsley[List[A]] =
    def go (items:List[A], forceNext:Boolean):Parsley[List[A]] = for {
      item <- if forceNext then sub.map(Some(_)) else option(attempt(sub))
      x <- item.match {
        case Some(item) => (attempt(char(',')) *> go(items :+ item, true)) <|> Parsley.pure(items :+ item)
        case None => Parsley.pure(items)
      }
    } yield x

    go(List.empty, false)

  val directMessageParser: Parsley[ServerMessage.DirectMessage] = for {
    _ <- attempt(string(":directMessage"))
    _ <- char(' ')
    userName <- UserName.parser
    _ <- char(' ')
    msg <- many(noneOf('\n'))
    x <- Parsley.pure(ServerMessage.DirectMessage(userName, msg.mkString))
  } yield x

  val roomMessageParser: Parsley[ServerMessage.RoomMessage] = for {
    _ <- attempt(string(":roomMessage"))
    _ <- char(' ')
    userName <- UserName.parser
    _ <- char(' ')
    roomName <- RoomName.parser
    _ <- char(' ')
    msg <- many(noneOf('\n'))
    x <- Parsley.pure(ServerMessage.RoomMessage(userName, roomName, msg.mkString))
  } yield x

  val allRoomNamesParser: Parsley[ServerMessage.AllRoomNames] = for
    _ <- attempt(string(":allRoomNames"))
    _ <- char(' ')
    roomNames <- listOfParser(RoomName.parser)
    x <- Parsley.pure(ServerMessage.AllRoomNames(roomNames.toSet))
  yield x

  val errorParser: Parsley[ServerMessage.Error] = for
    _ <- attempt(string(":error"))
    _ <- char(' ')

    err <- ServerErrorParser.parser
    x <- Parsley.pure(ServerMessage.Error(err))
  yield x


  val uuidParser: Parsley[UUID] =
    for
      b1 <- C.manyN(8, Character.alphaNum)
      _ <- char('-')
      b2 <- C.manyN(4, Character.alphaNum)
      _ <- char('-')
      b3 <- C.manyN(4, Character.alphaNum)
      _ <- char('-')
      b4 <- C.manyN(4, Character.alphaNum)
      _ <- char('-')
      b5 <- C.manyN(12, Character.alphaNum)
      s <- Parsley.pure(UUID.fromString(List(b1, b2, b3, b4, b5).map(_.mkString).mkString("-")))
    yield s



  val clientIdParser: Parsley[ClientId] =
    uuidParser.map(u => ClientId(u.toString()))

  val allRoomMembersParser: Parsley[ServerMessage.AllRoomMembers] =
    for
      _ <- attempt(string(":allRoomMembers"))
      _ <- char(' ')
      roomName <- RoomName.parser
      _ <- char(' ')
      userNames <- listOfParser(UserName.parser)
      x <- Parsley.pure(ServerMessage.AllRoomMembers(roomName, userNames.toSet))
    yield x

  val parser: Parsley[ServerMessage] = for {
    c <-
          acknowledgeParser
      <|> allRoomNamesParser
      <|> allRoomMembersParser
      <|> errorParser
      <|> directMessageParser
      <|> roomMessageParser
    _ <- eof
  } yield c
}

object ServerMessage {
  def parse (s:String):Option[ServerMessage] = ServerMessageParser.parser.parse(s).toOption

  final case class Error(error:ServerError) extends ServerMessage
  final case class Acknowledge(command:String) extends ServerMessage
  final case class AllRoomNames(roomNames:Set[RoomName]) extends ServerMessage
  final case class RoomMessage(from:UserName, roomName:RoomName, txt:String) extends ServerMessage
  final case class AllRoomMembers(roomName:RoomName, members:Set[UserName]) extends ServerMessage
  final case class DirectMessage(from:UserName, txt:String) extends ServerMessage
}

