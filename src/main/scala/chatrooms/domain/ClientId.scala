package chatrooms.domain

opaque type ClientId = String

object ClientId:
  def apply (v:String):ClientId = v

  extension (x : ClientId ) {
    def value : String = x
  }
