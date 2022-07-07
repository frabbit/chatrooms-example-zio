package chatrooms.domain

import zio.test._
import zio.test.Assertion.{equalTo}

private val spec_ = suite("ServerMessageSpec")(
  test("encode + parse should yield the original serverMessage") {
    check(Generators.serverMessage) { msg =>
      assert(ServerMessage.parse(ServerMessageEncoder.encode(msg)))(equalTo(Some(msg)))
    }
  } +
  test("...") {
    assert(true)(equalTo(true))
  }
).provideSomeLayer(zio.Random.live).provideSomeLayer(zio.test.Sized.live(100))

object ServerMessageSpec extends ZIOSpecDefault {
  override def spec = spec_
}
