ThisBuild / scalaVersion := "3.3.5"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.typerace"

lazy val pekkoVersion = "1.1.3"
lazy val pekkoHttpVersion = "1.1.0"
lazy val circeVersion = "0.14.10"

lazy val root = (project in file("."))
  .settings(
    name := "typing-race-server",
    libraryDependencies ++= Seq(
      // Pekko Typed + HTTP
      "org.apache.pekko" %% "pekko-actor-typed" % pekkoVersion,
      "org.apache.pekko" %% "pekko-stream"       % pekkoVersion,
      "org.apache.pekko" %% "pekko-http"         % pekkoHttpVersion,
      "org.apache.pekko" %% "pekko-slf4j"        % pekkoVersion,

      // JSON (WebSocket)
      "io.circe" %% "circe-core"    % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser"  % circeVersion,

      // Logging
      "ch.qos.logback" % "logback-classic" % "1.5.12",

      // Tests
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    Compile / run / mainClass := Some("com.typerace.Main")
  )
