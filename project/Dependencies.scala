import sbt._
import Keys._

object Dependencies {
  lazy val http4sVersion = "0.21.6"
  lazy val specs2Version = "4.10.0"

  val scala_213 = "2.13.2"
  val scala_212 = "2.12.11"

  lazy val http4sServer        = "org.http4s"                 %% "http4s-server"         % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"            % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blaze-server"   % http4sVersion
  lazy val http4sJetty         = "org.http4s"                 %% "http4s-servlet"        % http4sVersion
  lazy val http4sJson4sJackson = "org.http4s"                 %% "http4s-json4s-jackson" % http4sVersion
  lazy val http4sXmlInstances  = "org.http4s"                 %% "http4s-scala-xml"      % http4sVersion
  lazy val json4s              = "org.json4s"                 %% "json4s-ext"            % "3.6.9"
  lazy val json4sJackson       = "org.json4s"                 %% "json4s-jackson"        % json4s.revision
  lazy val swaggerModels       = "io.swagger"                  % "swagger-models"        % "1.6.2"
  lazy val swaggerCore         = "io.swagger"                  % "swagger-core"          % swaggerModels.revision
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"       % "1.2.3"
  lazy val uadetector          = "net.sf.uadetector"           % "uadetector-resources"  % "2014.10"
  lazy val shapeless           = "com.chuusai"                %% "shapeless"             % "2.3.3"
  lazy val scalaXml            = "org.scala-lang.modules"     %% "scala-xml"             % "1.3.0"
  lazy val swaggerUi           = "org.webjars"                 % "swagger-ui"            % "3.35.1"

  lazy val specs2              = Seq("org.specs2"              %% "specs2-core"          % specs2Version % "test",
                                     "org.specs2"              %% "specs2-scalacheck"    % specs2Version % "test" )

  lazy val `scala-reflect`     = "org.scala-lang"              % "scala-reflect"

  val silencerVersion = "1.7.0"
  lazy val silencerPlugin = compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full)
  lazy val silencerLib = "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full

  lazy val halDeps = libraryDependencies ++= Seq(json4sJackson)

  lazy val swaggerDeps = libraryDependencies ++= Seq(
    scalaXml,
    swaggerCore,
    swaggerModels,

    json4s % "test",
    json4sJackson % "test"
  )

  lazy val swaggerUiDeps = libraryDependencies ++= Seq(swaggerUi)

  lazy val exampleDeps = libraryDependencies ++= Seq(
    http4sBlaze,
    http4sDSL,
    json4s,
    json4sJackson,
    http4sJson4sJackson,
    uadetector
  )
}
