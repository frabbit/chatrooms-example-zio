
compile:
	sbt compile
test-unit:
	sbt test
test-e2e:
	sbt "E2ETest / test"