clean:
	bloop clean root
compile:
	bloop compile root
run-server:
	bloop run root -m chatrooms.socketapp.Server
run-client:
	sbt "runMain chatrooms.clientapp.Client"
compile-test-unit:
	bloop compile root-test
compile-test-e2e:
	bloop compile root-e2e
test-unit:
	bloop test root-test
test-unit-watch:
	bloop test root-test -w
test-e2e:
	bloop test root-e2e
test-e2e-watch:
	bloop test root-e2e -w
