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

case class SEAlreadyJoined() extends ServerError

object ServerErrorEncoder {
  def encode (c:ServerError):String = c.match {
    case SEAlreadyJoined() => "alreadyJoined"
  }
}

object ServerErrorParser {
  val alreadyJoinedParser: Parsley[SEAlreadyJoined] = for {
    _ <- attempt(string("alreadyJoined"))
    c <- Parsley.pure(SEAlreadyJoined())
  } yield c

  val parser: Parsley[ServerError] = for {
    c <- alreadyJoinedParser
    _ <- eof
  } yield c
}

sealed trait ServerMessage {
  def encode = ServerMessageEncoder.encode(this)
}

final case class SMError(error:ServerError) extends ServerMessage
final case class Acknowledge(command:String) extends ServerMessage
final case class AllRoomNames(roomNames:List[RoomName]) extends ServerMessage
final case class AllRoomMembers(roomName:RoomName, members:List[ClientId]) extends ServerMessage
final case class SMDirectMessage(from:UserName, txt:String) extends ServerMessage

object ServerMessageEncoder {
  def encode (c:ServerMessage):String = c.match {
    case SMError(error) => ":error " ++ error.encode
    case Acknowledge(name) => ":acknowledge " ++ name
    case AllRoomNames(names) => ":allRoomNames " ++ names.mkString(",")
    case AllRoomMembers(roomName:RoomName, members: List[ClientId]) => ":allRoomMembers " ++ roomName.value ++ " " ++ members.mkString(",")
    case SMDirectMessage(from, txt) => ":directMessage " ++ from.value ++ " " ++ txt
  }
}

object ServerMessageParser {
  val roomNameParser = RoomName.parser

  val acknowledgeParser: Parsley[Acknowledge] = for {
    _ <- attempt(string(":acknowledge"))
    _ <- char(' ')
    txt <- some(noneOf(' ', '\n'))
    x <- Parsley.pure(Acknowledge(txt.mkString))
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

  val directMessageParser: Parsley[SMDirectMessage] = for {
    _ <- attempt(string(":directMessage"))
    _ <- char(' ')
    userName <- UserName.parser
    _ <- char(' ')
    msg <- many(noneOf('\n'))
    x <- Parsley.pure(SMDirectMessage(userName, msg.mkString))
  } yield x

  val allRoomNamesParser: Parsley[AllRoomNames] = for
    _ <- attempt(string(":allRoomNames"))
    _ <- char(' ')
    roomNames <- listOfParser(RoomName.parser)
    x <- Parsley.pure(AllRoomNames(roomNames))
  yield x

  val errorParser: Parsley[SMError] = for
    _ <- attempt(string(":error"))
    _ <- char(' ')

    err <- ServerErrorParser.parser
    x <- Parsley.pure(SMError(err))
  yield x


  val uuidParser: Parsley[UUID] =
    for
      //cc853903-8e2d-4890-936d-8c7314773a6c
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
    for
      u <- uuidParser
    yield ClientId(u.toString())

  val allRoomMembersParser: Parsley[AllRoomMembers] =
    for
      _ <- attempt(string(":allRoomMembers"))
      _ <- char(' ')
      roomName <- RoomName.parser
      _ <- char(' ')
      clientIds <- listOfParser(clientIdParser)
      x <- Parsley.pure(AllRoomMembers(roomName, clientIds))
    yield x



  val parser: Parsley[ServerMessage] = for {
    c <-
      acknowledgeParser
      <|> allRoomNamesParser
      <|> allRoomMembersParser
      <|> errorParser
      <|> directMessageParser
    _ <- eof
  } yield c
}

object ServerMessage {
  def parse (s:String):Option[ServerMessage] = ServerMessageParser.parser.parse(s).toOption
}

