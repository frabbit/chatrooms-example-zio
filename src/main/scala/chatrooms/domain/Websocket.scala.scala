package chatrooms.domain

import zio.*

trait Websocket {
	def get(uri:String): Task[String]
}

object WebsocketImpl extends Websocket {
	def get (uri:String) = for {
		x <- ZIO.succeed("hello world")
	} yield (x)
}


object Websocket {
	val liveImpl:Websocket = new Websocket {
		def get (uri:String) = for {
			x <- ZIO.succeed("hello world")
		} yield (x)
	}


	val live = ZEnvironment(liveImpl)
}
