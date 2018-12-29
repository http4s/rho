import sbt._
import Keys._
import spray.revolver.RevolverPlugin._

import com.typesafe.sbt.SbtGit.git

import scala.util.Properties.envOrNone

import Dependencies._, RhoPlugin._

lazy val rho = project
  .in(file("."))
  .settings(buildSettings: _*)
  .aggregate(`rho-core`, `rho-hal`, `rho-swagger`, `rho-examples`)

lazy val `rho-core` = project
  .in(file("core"))
  .settings(buildSettings: _*)

lazy val `rho-hal` = project
  .in(file("hal"))
  .settings(buildSettings :+ halDeps: _*)
  .dependsOn(`rho-core`)

lazy val `rho-swagger` = project
  .in(file("swagger"))
  .settings(buildSettings :+ swaggerDeps: _*)
  .dependsOn(`rho-core` % "compile->compile;test->test")

lazy val docs = project
  .in(file("docs"))
  .settings(buildSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(SiteScaladocPlugin)
  .enablePlugins(GhpagesPlugin)
  .settings(Seq(
    dontPublish,
    description := "Api Documentation",
    autoAPIMappings := true,
    scalacOptions in Compile := scaladocOptions(
      (baseDirectory in ThisBuild).value,
      version.value,
      apiVersion.value
    ),
    scalacOptions in (ScalaUnidoc, unidoc) += "-Ypartial-unification",
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(
      `rho-core`,
      `rho-hal`,
      `rho-swagger`
    ),
    git.remoteRepo := "git@github.com:http4s/rho.git",
    ghpagesCleanSite := VersionedGhPages.cleanSite0.value,
    ghpagesSynchLocal := VersionedGhPages.synchLocal0.value,
    mappings in makeSite := {
      val (major, minor) = apiVersion.value
      for {
        (f, d) <- (mappings in (ScalaUnidoc, packageDoc)).value
      } yield (f, s"api/$major.$minor/$d")
    }
  ))
  .dependsOn(`rho-core`, `rho-hal`, `rho-swagger`)

lazy val `rho-examples` = project
  .in(file("examples"))
  .settings(
    buildSettings ++
      Revolver.settings ++
      Seq(
        exampleDeps,
        libraryDependencies ++= Seq(logbackClassic, http4sXmlInstances),
        dontPublish
      ): _*)
  .dependsOn(`rho-swagger`, `rho-hal`)

lazy val compileFlags = Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ywarn-unused",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

/* Don't publish setting */
lazy val dontPublish = packagedArtifacts := Map.empty

lazy val license = licenses in ThisBuild := Seq(
  "Apache License, Version 2.0" -> url(
    "http://www.apache.org/licenses/LICENSE-2.0.txt")
)

lazy val buildSettings = publishing ++
  Seq(
    scalaVersion := "2.12.6",
    crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
    scalacOptions ++= compileFlags,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    fork in run := true,
    organization in ThisBuild := "org.http4s",
    homepage in ThisBuild := Some(url(homepageUrl)),
    description := "A self documenting DSL build upon the http4s framework",
    license,
    libraryDependencies ++= Seq(
      shapeless,
      http4sServer % "provided",
      logbackClassic % "test"
    ),
    libraryDependencies ++= specs2,
    libraryDependencies += `scala-reflect` % scalaVersion.value,
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.8" cross CrossVersion.binary)
  )

// to keep REPL usable
scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

lazy val publishing = Seq(
  extras,
  credentials ++= travisCredentials.toSeq,
  publishMavenStyle in ThisBuild := true,
  publishArtifact in (ThisBuild, Test) := false,
  // Don't publish root pom.  It's not needed.
  packagedArtifacts in LocalRootProject := Map.empty,
  publishArtifact in Test := false,
  publishTo in ThisBuild := Some(nexusRepoFor(version.value)),
  scmInfo in ThisBuild := {
    val base = "github.com/http4s/rho"
    Some(
      ScmInfo(url(s"https://$base"),
              s"scm:git:https://$base",
              Some(s"scm:git:git@$base")))
  }
)

lazy val travisCredentials =
  (envOrNone("SONATYPE_USERNAME"), envOrNone("SONATYPE_PASSWORD")) match {
    case (Some(user), Some(pass)) =>
      Some(
        Credentials("Sonatype Nexus Repository Manager",
                    "oss.sonatype.org",
                    user,
                    pass))
    case _ =>
      None
  }

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
