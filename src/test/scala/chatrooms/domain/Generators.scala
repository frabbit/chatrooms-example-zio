package chatrooms.domain

import zio.test.Gen
import zio.Random
import zio.test.Sized



object Generators {

  def clientId : Gen[Random with Sized, ClientId] = Gen.uuid.map(id => ClientId(id.toString()))
  def client : Gen[Random with Sized, Client] = clientId.zip(userName).map(Client.apply)
  def roomName : Gen[Random with Sized, RoomName] = Gen.stringBounded(1, 30)(Gen.alphaNumericChar.filter(c => !List('\n', ' ').contains(c) )).map(s => RoomName(s))
  def userName : Gen[Random with Sized, UserName] = Gen.stringBounded(1, 30)(Gen.alphaNumericChar.filter(c => !List('\n', ' ').contains(c) )).map(s => UserName(s))
  def command: Gen[Random with Sized, Command] =
    val txtGen = Gen.string1(Gen.alphaNumericChar.filter(c => c != '\n'))
    Gen.oneOf(
      roomName.map(JoinRoom(_)),
      roomName.map(LeaveRoom(_)),
      roomName.zip(txtGen).map(SendText.apply),
      roomName.map(SendPing(_)),
      roomName.map(ListRoomMembers(_)),
      userName.map(Join(_)),
      Gen.const(ListRooms()),
    )
  def serverMessage: Gen[Random with Sized, ServerMessage] =
    val cmdGen = Gen.string1(Gen.alphaNumericChar.filter(c => c != '\n'))
    val clientIdList = Gen.listOfBounded(1, 30)(clientId)
    val roomNameList = Gen.listOfBounded(0, 50)(roomName)
    Gen.oneOf(
      cmdGen.map(Acknowledge(_)),
      roomNameList.map(AllRoomNames(_)),
      roomName.zip(clientIdList).map(AllRoomMembers.apply),
    )
}
