import sbt._
import Keys._
import spray.revolver.RevolverPlugin._

import com.typesafe.sbt.SbtGhPages.ghpages
import com.typesafe.sbt.SbtGhPages.GhPagesKeys

import com.typesafe.sbt.SbtSite.SiteKeys.siteMappings
import com.typesafe.sbt.SbtSite.site

import com.typesafe.sbt.SbtGit.git

import sbtunidoc.Plugin.ScalaUnidoc
import sbtunidoc.Plugin.unidocSettings
import sbtunidoc.Plugin.UnidocKeys._

import scala.util.Properties.envOrNone

object RhoBuild extends Build {
  import Dependencies._

  val homepageUrl= "https://github.com/http4s/rho"

  val apiVersion = TaskKey[(Int, Int)]("api-version", "Defines the API compatibility version for the project.")

  lazy val rho = project
                  .in(file("."))
                  .settings(buildSettings: _*)
                  .aggregate(`rho-core`, `rho-hal`, `rho-swagger`, `rho-examples`)

  lazy val `rho-core` = project
                    .in(file("core"))
                    .settings(buildSettings: _*)

  lazy val `rho-hal` = project
                   .in(file("hal"))
                   .settings(buildSettings:+ halDeps : _*)
                   .dependsOn(`rho-core`)

  lazy val `rho-swagger` = project
                      .in(file("swagger"))
                      .settings(buildSettings:+ swaggerDeps : _*)
                      .dependsOn(`rho-core` % "compile->compile;test->test")

  lazy val docs = project
              .in(file("docs"))
              .settings(buildSettings)
              .settings(unidocSettings)
              .settings(ghpages.settings ++ site.settings)
              .settings(site.includeScaladoc())
              .settings(Seq(
                dontPublish,
                description := "Api Documentation",
                autoAPIMappings := true,
                scalacOptions in Compile <++= (baseDirectory in ThisBuild, version, apiVersion).map(
                  scaladocOptions(_, _, _)
                ),
                unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(
                  `rho-core`,
                  `rho-hal`,
                  `rho-swagger`
                ),
                git.remoteRepo := "git@github.com:http4s/rho.git",
                GhPagesKeys.cleanSite <<= VersionedGhPages.cleanSite0,
                GhPagesKeys.synchLocal <<= VersionedGhPages.synchLocal0,
                siteMappings <++= (mappings in (ScalaUnidoc, packageDoc), apiVersion) map { (m, api) =>
                    val (major,minor) = api
                    for ((f, d) <- m) yield (f, s"api/$major.$minor/$d")
                }
                ))
              .dependsOn(`rho-core`, `rho-hal`, `rho-swagger`)

  lazy val `rho-examples` = project
                        .in(file("examples"))
                        .settings(buildSettings ++
                                  Revolver.settings ++
                                  Seq(
                                    exampleDeps,
                                    libraryDependencies ++= Seq(logbackClassic, http4sXmlInstances),
                                    dontPublish
                                  ) :_*)
                        .dependsOn(`rho-swagger`, `rho-hal`)

  lazy val compileFlags = Seq("-feature") //, "-Xlog-implicits")

    /* Don't publish setting */
  val dontPublish = packagedArtifacts := Map.empty

  lazy val license = licenses in ThisBuild := Seq(
    "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")
  )

  lazy val buildSettings = publishing ++
     Seq(
        scalaVersion := "2.11.11",
        crossScalaVersions := Seq("2.11.11", "2.12.3"),
        scalacOptions ++= compileFlags,
        resolvers += Resolver.sonatypeRepo("snapshots"),
        fork in run := true,

        organization in ThisBuild := "org.http4s",
        homepage in ThisBuild := Some(url(homepageUrl)),
        description := "A self documenting DSL build upon the http4s framework",
        license,

        libraryDependencies ++= Seq(
          shapeless,
          http4sServer     % "provided",
          logbackClassic   % "test"
        ),
        libraryDependencies ++= specs2,
        libraryDependencies += `scala-reflect` % scalaVersion.value
    )

  lazy val publishing = Seq(
    extras,
    credentials ++= travisCredentials.toSeq,
    publishMavenStyle in ThisBuild := true,
    publishArtifact in (ThisBuild, Test) := false,
    // Don't publish root pom.  It's not needed.
    packagedArtifacts in file(".") := Map.empty,
    publishArtifact in Test := false,
    publishTo in ThisBuild <<= version(v => Some(nexusRepoFor(v))),
    scmInfo in ThisBuild := {
      val base = "github.com/http4s/rho"
      Some(ScmInfo(url(s"https://$base"), s"scm:git:https://$base", Some(s"scm:git:git@$base")))
    }
  )

  lazy val travisCredentials = (envOrNone("SONATYPE_USERNAME"), envOrNone("SONATYPE_PASSWORD")) match {
    case (Some(user), Some(pass)) =>
      Some(Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass))
    case _ =>
      None
  }

  /** Some helper functions **************************************/
  def nexusRepoFor(version: String): Resolver = {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot(version)) "snapshots" at nexus + "content/repositories/snapshots"
    else "releases" at nexus + "service/local/staging/deploy/maven2"
  }

  def extractApiVersion(version: String) = {
    val VersionExtractor = """(\d+)\.(\d+)\..*""".r
    version match {
      case VersionExtractor(major, minor) => (major.toInt, minor.toInt)
    }
  }

  def scaladocOptions(base: File, version: String, apiVersion: (Int, Int)): List[String] = {
    val sourceLoc =
      if (version.endsWith("SNAPSHOT")) {
        s"$homepageUrl/tree/master€{FILE_PATH}.scala"
      } else {
        val (major,minor) = apiVersion
        s"$homepageUrl/tree/v$major.$minor.0€{FILE_PATH}.scala"
      }

    val opts = List("-implicits",
                    "-doc-source-url", sourceLoc,
                    "-sourcepath", base.getAbsolutePath
                    )
    opts
  }

  def isSnapshot(version: String): Boolean = version.endsWith("-SNAPSHOT")

  lazy val extras = pomExtra in ThisBuild := (
    <developers>
      <developer>
        <id>brycelane</id>
        <name>Bryce L. Anderson</name>
        <email>bryce.anderson22@gmail.com</email>
      </developer>
      <developer>
        <id>before</id>
        <name>André Rouél</name>
      </developer>
      <developer>
        <id>rossabaker</id>
        <name>Ross A. Baker</name>
        <email>ross@rossabaker.com</email>
      </developer>
    </developers>
    )

}

object Dependencies {
  import RhoBuild._

  lazy val http4sVersion = s"0.17.0-RC1"
  lazy val specs2Version = "3.8.6"

  lazy val http4sServer        = "org.http4s"                 %% "http4s-server"         % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"            % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blaze-server"   % http4sVersion
  lazy val http4sJetty         = "org.http4s"                 %% "http4s-servlet"        % http4sVersion
  lazy val http4sJson4sJackson = "org.http4s"                 %% "http4s-json4s-jackson" % http4sVersion
  lazy val http4sXmlInstances  = "org.http4s"                 %% "http4s-scala-xml"      % http4sVersion
  lazy val json4s              = "org.json4s"                 %% "json4s-ext"            % "3.5.0"
  lazy val json4sJackson       = "org.json4s"                 %% "json4s-jackson"        % json4s.revision
  lazy val swaggerModels       = "io.swagger"                  % "swagger-models"        % "1.5.10"
  lazy val swaggerCore         = "io.swagger"                  % "swagger-core"          % swaggerModels.revision
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"       % "1.1.3"
  lazy val uadetector          = "net.sf.uadetector"           % "uadetector-resources"  % "2014.09"
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
