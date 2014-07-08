import sbt._
import Keys._

object MyBuild extends Build {
  import Dependencies._


  lazy val rho = project
                  .in(file("."))
                  .settings(buildSettings: _*)
                  .aggregate(core, hal, swagger)
   
  lazy val core = project
                    .in(file("core"))
                    .settings(buildSettings: _*)

  lazy val hal = project
                   .in(file("hal"))
                   .settings(buildSettings:+ halDeps : _*)
                   .dependsOn(core)

  lazy val swagger = project
                      .in(file("swagger"))
                      .settings(buildSettings:+ swaggerDeps : _*)
                      .dependsOn(core)

  lazy val compileFlags = Seq("-feature")

  lazy val rhoVersion = "0.1.0-SNAPSHOT"

  lazy val buildSettings = Defaults.defaultSettings ++
     Seq(
        scalaVersion := "2.11.1",
        scalacOptions ++= compileFlags,
        version := rhoVersion,
        resolvers += Resolver.sonatypeRepo("snapshots"),
        resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
        fork in run := true,
        libraryDependencies ++= Seq(
          http4sCore,
          logbackClassic % "test",
          specs2 % "test"
        )
    )

}

object Dependencies {
  lazy val http4sVersion = "0.2.0-SNAPSHOT"

  lazy val http4sCore          = "org.http4s"                 %% "http4s-core"         % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"          % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blaze"        % http4sVersion
  lazy val http4sJetty         = "org.http4s"                 %% "http4s-servlet"      % http4sVersion
  lazy val config              = "com.typesafe"                % "config"              % "1.2.1"
  lazy val json4sJackson       = "org.json4s"                 %% "json4s-jackson"      % "3.2.10"
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"     % "1.1.2"
  lazy val scalaloggingSlf4j   = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  lazy val specs2              = "org.specs2"                 %% "specs2"              % "2.3.12"

  lazy val halDeps = libraryDependencies ++= Seq(json4sJackson)

  lazy val swaggerDeps = libraryDependencies ++= Seq(
    json4sJackson,
    "org.json4s" %% "json4s-ext"     % "3.2.10"
  )

}
