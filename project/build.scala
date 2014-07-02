import sbt._
import Keys._

object MyBuild extends Build {
  import Dependencies._

  lazy val compileFlags = Seq("-feature")

  lazy val buildSettings = Defaults.defaultSettings ++
     Seq(
        scalaVersion := "2.11.1",
        scalacOptions ++= compileFlags,
        resolvers += Resolver.sonatypeRepo("snapshots"),
        resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
        fork in run := true,
        libraryDependencies ++= Seq(
          http4sCore,
          http4sDSL,
          http4sBlaze,
          specs2 % "test"
        ) ++ swaggerDeps
    )

  lazy val myProject = Project(
    id = "http4s-rho",
    base = file("."),
    settings = buildSettings ++ Seq(version := "0.1.0-SNAPSHOT")
  )

}


object Dependencies {
  lazy val http4sVersion = "0.2.0-SNAPSHOT"

  lazy val http4sCore          = "org.http4s"                 %% "http4s-core"         % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"          % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blaze"        % http4sVersion
  lazy val http4sJetty         = "org.http4s"                 %% "http4s-servlet"      % http4sVersion
  lazy val config              = "com.typesafe"                % "config"              % "1.2.1"
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"     % "1.1.2"
  lazy val scalaloggingSlf4j   = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  lazy val specs2              = "org.specs2"                 %% "specs2"              % "2.3.12"

  lazy val swaggerDeps = Seq(
    "org.json4s" %% "json4s-jackson" % "3.2.10",
    "org.json4s" %% "json4s-ext"     % "3.2.10"
  )
  
}
