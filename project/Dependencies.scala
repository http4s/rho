import sbt._
import Keys._

// format: off
object Dependencies {
  val http4sVersion = "1.0.0-M35"
  val circeVersion = "0.14.2"

  val scala_213 = "2.13.8"


  lazy val circeCore           = "io.circe"                   %% "circe-core"            % circeVersion
  lazy val circeGeneric        = "io.circe"                   %% "circe-generic"         % circeVersion
  lazy val circeParser         = "io.circe"                   %% "circe-parser"          % circeVersion
  lazy val http4sServer        = "org.http4s"                 %% "http4s-server"         % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"            % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blaze-server"   % http4sVersion
  lazy val http4sCirce         = "org.http4s"                 %% "http4s-circe"          % http4sVersion
  lazy val http4sXmlInstances  = "org.http4s"                 %% "http4s-scala-xml"      % http4sVersion
  lazy val swaggerModels       = "io.swagger"                  % "swagger-models"        % "1.6.6"
  lazy val swaggerCore         = "io.swagger"                  % "swagger-core"          % swaggerModels.revision
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"       % "1.2.11"
  lazy val uadetector          = "net.sf.uadetector"           % "uadetector-resources"  % "2014.10"
  lazy val shapeless           = "com.chuusai"                %% "shapeless"             % "2.3.9"
  lazy val scalaXml            = "org.scala-lang.modules"     %% "scala-xml"             % "2.1.0"
  lazy val swaggerUi           = "org.webjars"                 % "swagger-ui"            % "3.52.5"
  lazy val munit               = "org.scalameta"              %% "munit"                 % "0.7.29"         % "test"
  lazy val munitCatsEffect     = "org.typelevel"              %% "munit-cats-effect-3"   % "1.0.7"          % "test"
  lazy val scalacheckMunit     = "org.scalameta"              %% "munit-scalacheck"      % munit.revision   % "test"

  lazy val `scala-reflect`     = "org.scala-lang"              % "scala-reflect"

  val silencerVersion = "1.7.9"
  lazy val silencerPlugin = compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full)
  lazy val silencerLib = "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
  lazy val kindProjector = compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

  lazy val halDeps = libraryDependencies ++= Seq(http4sCirce)

  lazy val swaggerDeps = libraryDependencies ++= Seq(
    scalaXml,
    swaggerCore,
    swaggerModels,

    http4sCirce % "test",
    circeParser % "test",
    circeGeneric % "test"
  )

  lazy val swaggerUiDeps = libraryDependencies ++= Seq(swaggerUi)

  lazy val exampleDeps = libraryDependencies ++= Seq(
    circeCore,
    circeGeneric,
    circeParser,
    http4sBlaze,
    http4sDSL,
    http4sCirce,
    http4sXmlInstances,
    logbackClassic, 
    uadetector
  )
}
