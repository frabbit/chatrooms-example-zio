package chatrooms.domain

import zio.test._
import zio.test.Assertion.{equalTo}

private val spec_ = suite("CommandSpec")(
  test("encode + parse should yield the original command") {
    check(Generators.command) { command =>
      assert(Command.parse(CommandEncoder.encode(command)))(equalTo(Some(command)))
    }
  }
).provideSomeLayer(zio.Random.live).provideSomeLayer(zio.test.Sized.live(100))

object CommandSpec extends ZIOSpecDefault {
  override def spec = spec_
}
