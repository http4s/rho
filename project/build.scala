import sbt._
import Keys._
import spray.revolver.RevolverPlugin._

object MyBuild extends Build {
  import Dependencies._


  lazy val rho = project
                  .in(file("."))
                  .settings(buildSettings: _*)
                  .aggregate(core, hal, swagger, examples)
   
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
                      .dependsOn(core % "compile->compile;test->test")

  lazy val examples = project
                        .in(file("examples"))
                        .settings(buildSettings ++ Revolver.settings :+ exampleDeps :_*)
                        .dependsOn(swagger, hal)

  lazy val compileFlags = Seq("-feature")

  lazy val rhoVersion = "0.1.0-SNAPSHOT"

  lazy val buildSettings = Defaults.defaultSettings ++
     Seq(
        scalaVersion := "2.11.2",
        scalacOptions ++= compileFlags,
        version := rhoVersion,
        resolvers += Resolver.sonatypeRepo("snapshots"),
        resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
        fork in run := true,
        libraryDependencies ++= Seq(
          http4sServer,
          logbackClassic % "test",
          scalazSpecs2 % "test"
        )
    )

}

object Dependencies {
  lazy val http4sVersion = "0.3.0"

//  lazy val http4sCore          = "org.http4s"                 %% "http4s-core"         % http4sVersion
  lazy val http4sServer        = "org.http4s"                 %% "http4s-server"       % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"          % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blazeserver"  % http4sVersion
  lazy val http4sJetty         = "org.http4s"                 %% "http4s-servlet"      % http4sVersion
  lazy val http4sJson4sJackson = "org.http4s"                 %% "http4s-json4s-jackson" % http4sVersion
  lazy val config              = "com.typesafe"                % "config"              % "1.2.1"
  lazy val json4s              = "org.json4s"                 %% "json4s-ext"          % "3.2.10"
  lazy val json4sJackson       = "org.json4s"                 %% "json4s-jackson"      % "3.2.10"
  lazy val swaggerCore         = "com.wordnik"                %% "swagger-core"        % "1.3.8"
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"     % "1.1.2"
  lazy val scalaloggingSlf4j   = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  lazy val scalazSpecs2        = "org.typelevel"              %% "scalaz-specs2"       % "0.3.0"

  lazy val halDeps = libraryDependencies ++= Seq(json4sJackson)

  lazy val swaggerDeps = libraryDependencies ++= Seq(
    swaggerCore,
    json4sJackson,
    json4s
  )

  lazy val exampleDeps = libraryDependencies ++= Seq(
    http4sBlaze,
    http4sJson4sJackson
  )
}
