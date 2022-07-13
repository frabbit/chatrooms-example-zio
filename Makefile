
compile:
	sbt compile
test-unit:
	sbt test
test-unit-watch:
	sbt ~test
test-e2e:
	sbt "E2ETest / test"
test-e2e-watch:
	sbt "~E2ETest / test"