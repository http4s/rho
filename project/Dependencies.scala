import sbt._
import Keys._

object Dependencies {
  lazy val http4sVersion = "0.18.0-M4"
  lazy val specs2Version = "4.0.1"

  lazy val http4sServer        = "org.http4s"                 %% "http4s-server"         % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"            % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blaze-server"   % http4sVersion
  lazy val http4sJetty         = "org.http4s"                 %% "http4s-servlet"        % http4sVersion
  lazy val http4sJson4sJackson = "org.http4s"                 %% "http4s-json4s-jackson" % http4sVersion
  lazy val http4sXmlInstances  = "org.http4s"                 %% "http4s-scala-xml"      % http4sVersion
  lazy val json4s              = "org.json4s"                 %% "json4s-ext"            % "3.5.3"
  lazy val json4sJackson       = "org.json4s"                 %% "json4s-jackson"        % json4s.revision
  lazy val swaggerModels       = "io.swagger"                  % "swagger-models"        % "1.5.16"
  lazy val swaggerCore         = "io.swagger"                  % "swagger-core"          % swaggerModels.revision
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"       % "1.2.3"
  lazy val uadetector          = "net.sf.uadetector"           % "uadetector-resources"  % "2014.10"
  lazy val shapeless           = "com.chuusai"                %% "shapeless"             % "2.3.2"

  lazy val specs2              = Seq("org.specs2"              %% "specs2-core"          % specs2Version % "test",
                                     "org.specs2"              %% "specs2-scalacheck"    % specs2Version % "test" )

  lazy val `scala-reflect`     = "org.scala-lang"              % "scala-reflect"


  lazy val halDeps = libraryDependencies ++= Seq(json4sJackson)

  lazy val swaggerDeps = libraryDependencies ++= Seq(
    json4s,
    json4sJackson,
    swaggerCore,
    swaggerModels
  )

  lazy val exampleDeps = libraryDependencies ++= Seq(
    http4sBlaze,
    http4sDSL,
    json4s,
    json4sJackson,
    http4sJson4sJackson,
    uadetector
  )
}
