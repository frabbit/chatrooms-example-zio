package chatrooms.domain

import parsley.Parsley
import parsley.combinator._
import parsley.character._

opaque type UserName = String

object UserName:
  def apply (v:String):UserName = v

  extension (x : UserName ) {
    def value : String = x
  }

  val parser: Parsley[UserName] = for {
    userName <- some(noneOf(' ', '\n', ','))
  } yield UserName(userName.mkString)