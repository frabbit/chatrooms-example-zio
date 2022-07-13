package chatrooms.domain

import parsley.Parsley
import parsley.combinator._
import parsley.character._

opaque type RoomName = String

object RoomName:
  def apply (v:String):RoomName = v

  extension (x : RoomName ) {
    def value : String = x
    def encode = x.value
  }

  val parser: Parsley[RoomName] = for {
    roomName <- some(noneOf(' ', '\n', ','))
  } yield RoomName(roomName.mkString)