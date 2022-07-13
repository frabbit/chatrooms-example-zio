package chatrooms.domain

import zio.test._
import zio.test.Assertion.{equalTo}

object ServerMessageSpec extends ZIOSpecDefault {

  private val spec_ = suite("ServerMessageSpec")(
    test("encode + parse should yield the original serverMessage") {
      check(Generators.serverMessage) { msg =>
        assert(ServerMessage.parse(ServerMessageEncoder.encode(msg)))(equalTo(Some(msg)))
      }
    } +
    test("...") {
      assert(true)(equalTo(true))
    }
  ) @@ _root_.chatrooms.CustomTestConfig.use

  override def spec = spec_
}
