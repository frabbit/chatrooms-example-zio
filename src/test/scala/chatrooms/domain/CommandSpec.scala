package chatrooms.domain

import zio.test._
import zio.test.Assertion.{equalTo}

object CommandSpec extends ZIOSpecDefault {
  private val spec_ = suite("CommandSpec")(
    test("encode + parse should yield the original command") {
      check(Generators.command) { command =>
        assert(Command.parse(CommandEncoder.encode(command)))(equalTo(Some(command)))
      }
    }
  ) @@ _root_.chatrooms.CustomTestConfig.use

  override def spec = spec_
}
