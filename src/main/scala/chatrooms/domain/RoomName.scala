package chatrooms.domain

opaque type RoomName = String

object RoomName:
  def apply (v:String):RoomName = v

  extension (x : RoomName ) {
    def value : String = x
  }