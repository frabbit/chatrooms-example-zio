# Chatrooms

![ci workflow](https://github.com/frabbit/chatrooms-example-zio/actions/workflows/ci.yml/badge.svg)

Chatrooms is a simple Chat-Server written in Scala. It allows clients to communicate with each other in different rooms. The protocol is very simple, clients send commands to the server and the server sends ServerMessages to the clients. I wrote this application in order to improve my Scala and ZIO knowledge. 

# Usage

The project can be compiled and tested with `sbt` or `bloop`. Make sure to export the build for bloop with `sbt bloopInstall` initially. The `Makefile` contains common build commands. You can compile the application with `make compile` and run the web server with `make run-server`. A simple client can be started with make `make run-client`. The Client can only be run with sbt, because [bloop has problems with reading input from stdin](https://github.com/scalacenter/bloop/issues/882).

# Test

This project has unit and e2e tests. The unit tests can be run with `make test-unit` and the e2e tests with `make test-e2e`. The e2e tests use a custom client to send and receive messages. 

# Architecture

The application follows the [ports and adapters (hexagonal) architecture pattern](https://alistair.cockburn.us/hexagonal-architecture/). The input or driving ports can be found in `src/main/scala/chatrooms/usecases`. These are the entry points into the business logic of the application. Driven Ports like `MessageService` are part of the domain layer in `src/main/scala/chatrooms/domain`. Implementations of driven ports can be found in `main/scala/chatrooms/socketapp`.