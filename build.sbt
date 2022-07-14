import bloop.integrations.sbt.BloopDefaults

enablePlugins(JavaAppPackaging)

val scala3Version = "3.1.0"

lazy val E2ETest = config("e2e") extend(Test)


val uzhttp = "org.polynote" %% "uzhttp" % "0.3.0-RC1"

val sttpVersion = "3.6.2"
val zioVersion = "2.0.0-RC6"

val sttp = "com.softwaremill.sttp.client3" %% "core" % sttpVersion
val sttpzio  = "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion
//val sttpziostreams  = "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio-streams" % sttpVersion
val zioTest    = "dev.zio" %% "zio-test"     % zioVersion % "test,e2e"
val zioTestMagnolia    = "dev.zio" %% "zio-test-magnolia"     % zioVersion % "test,e2e"
val zioTestSbt = "dev.zio" %% "zio-test-sbt" % zioVersion % "test,e2e"
val zioMock = "dev.zio" %% "zio-mock" % "1.0.0-RC6"

val zio = "dev.zio" %% "zio" % zioVersion
val zioStreams = "dev.zio" %% "zio-streams" % zioVersion



lazy val root = project
  .in(file("."))
  .configs(E2ETest)
  .settings(
    inConfig(E2ETest)(Defaults.testSettings ++ BloopDefaults.configSettings),
    autoCompilerPlugins := true,
    name := "chatrooms",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    exportJars := true,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    E2ETest/testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    Compile/packageBin/mainClass := Some("chatrooms.socketapp.Server"),
    libraryDependencies ++= Seq(
      "io.d11" %% "zhttp"      % "2.0.0-RC9",
      "org.scalameta" %% "munit" % "0.7.29" % "test,e2e",
      zio,
      zioStreams,
      "org.slf4j" % "slf4j-api" % "1.7.36",
      "org.slf4j" % "slf4j-simple" % "1.7.36",
      "com.github.j-mie6" %% "parsley" % "3.3.9",
      "org.jline" % "jline" % "3.21.0",
      uzhttp,
      sttp,
      sttpzio,
      zioTest,
      zioTestSbt,
      zioTestMagnolia,
      zioMock,
    )
  )
