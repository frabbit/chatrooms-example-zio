package chatrooms.domain

import zio.*

trait Http {
	def get(uri:String): Task[String]
}

object Http {
	val mockImpl:Http = new Http {
		def get (uri:String) = for {
			x <- ZIO.succeed("hello world")
		} yield (x)
	}

	def get: String => ZIO[Http, Throwable, String] = (uri:String) => for {
		http <- ZIO.service[Http]
		r <- http.get(uri)
	} yield r

	val mock:ZLayer[Any, Nothing, Http] = ZLayer.succeed(mockImpl)
}
