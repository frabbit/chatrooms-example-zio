package chatrooms.domain

import zio.test.Gen
import zio.Random
import zio.test.Sized

object Generators {
  def message : Gen[Random with Sized, String] = Gen.string1(Gen.char.filter(c => !List('\n').contains(c) ))
  def clientId : Gen[Random with Sized, ClientId] = Gen.uuid.map(id => ClientId(id.toString()))
  def client : Gen[Random with Sized, Client] = clientId.zip(userName).map(Client.apply)
  def roomName : Gen[Random with Sized, RoomName] = Gen.stringBounded(1, 30)(Gen.alphaNumericChar.filter(c => !List('\n', ' ').contains(c) )).map(s => RoomName(s))
  def userName : Gen[Random with Sized, UserName] = Gen.stringBounded(1, 30)(Gen.alphaNumericChar.filter(c => !List('\n', ' ').contains(c) )).map(s => UserName(s))
  def command: Gen[Random with Sized, Command] =
    val txtGen = Gen.string1(Gen.alphaNumericChar.filter(c => c != '\n'))
    Gen.oneOf(
      roomName.map(Command.JoinRoom(_)),
      roomName.map(Command.LeaveRoom(_)),
      roomName.zip(txtGen).map(Command.SendText.apply),
      roomName.map(Command.SendPing(_)),
      roomName.map(Command.ListRoomMembers(_)),
      userName.map(Command.Join(_)),
      roomName.zip(message).map(Command.SendMessageToRoom.apply),
      userName.zip(message).map(Command.SendDirectMessage.apply),
      Gen.const(Command.ListRooms),
    )
  def serverMessage: Gen[Random with Sized, ServerMessage] =
    val errorGen = Gen.oneOf(
      Gen.const(ServerError.AlreadyJoined),
      Gen.const(ServerError.UserNameTaken),
      Gen.const(ServerError.UserIsNotInRoom),
      roomName.map(ServerError.RoomNotFound(_))
      )
    val cmdGen = Gen.string1(Gen.alphaNumericChar.filter(c => c != '\n'))
    val userNames = Gen.listOfBounded(1, 30)(userName).map(_.toSet)
    val roomNames = Gen.listOfBounded(0, 50)(roomName).map(_.toSet)
    Gen.oneOf(
      errorGen.map(ServerMessage.Error(_)),
      cmdGen.map(ServerMessage.Acknowledge(_)),
      roomNames.map(ServerMessage.AllRoomNames(_)),
      roomName.zip(userNames).map(ServerMessage.AllRoomMembers.apply),
      userName.zip(message).map(ServerMessage.DirectMessage.apply),
      userName.zip(roomName).zip(message).map(ServerMessage.RoomMessage.apply),
    )
}