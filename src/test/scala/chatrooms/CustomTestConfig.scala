package chatrooms

import zio.test.TestConfig
import zio.test.TestAspect

object CustomTestConfig {
  val use = TestAspect.samples(20)
    @@ TestAspect.retries(100)
    @@ TestAspect.shrinks(1000)
    @@ TestAspect.repeats(100)
    @@ TestAspect.timed

}