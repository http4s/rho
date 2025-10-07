import sbt._
import Keys._

import Dependencies._

ThisBuild / tlBaseVersion := "0.23"
ThisBuild / startYear := Some(2014)
ThisBuild / scalaVersion := scala_213
ThisBuild / crossScalaVersions := Seq(scala_213)

// tlCiScalafixCheck is insufficient, because sbt-http4s-org-2.0.0 hardcodes its own
ThisBuild / githubWorkflowBuildPostamble ~= { old =>
  old.filterNot(_.name == Some("Check scalafix lints"))
}

lazy val rho = project
  .in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(buildSettings: _*)
  .settings(libraryDependencies := Seq.empty)
  .aggregate(`rho-core`, `rho-swagger`, `rho-swagger-ui`, `rho-examples`)

lazy val `rho-core` = project
  .in(file("core"))
  .settings(buildSettings)
  .settings(
    Compile / unmanagedSourceDirectories ++= {
      val baseDir = baseDirectory.value

      val mainSrcDir = "src/main/scala"
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, minor)) if minor <= 12 => Seq(baseDir / s"$mainSrcDir-2.12-")
        case Some((2, minor)) if minor >= 13 => Seq(baseDir / s"$mainSrcDir-2.13+")
        case _ => Nil
      }
    },
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.14.0",
      http4sCore,
      http4sServer % Test
    )
  )

lazy val `rho-swagger` = project
  .in(file("swagger"))
  .settings(buildSettings :+ swaggerDeps: _*)
  .dependsOn(`rho-core` % "compile->compile;test->test")

lazy val `rho-swagger-ui` = project
  .in(file("swagger-ui"))
  .settings(buildSettings :+ swaggerUiDeps: _*)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey]("swaggerUiVersion" -> Dependencies.swaggerUi.revision),
    buildInfoPackage := "org.http4s.rho.swagger.ui"
  )
  .dependsOn(`rho-swagger`)

// TODO no site is published as of 2025-06-07
lazy val docs = project
  .in(file("docs"))
  .settings(buildSettings)
  // .enablePlugins(Http4sOrgSitePlugin)
  .settings(
    dontPublish,
    description := "Api Documentation",
    autoAPIMappings := true
  )
  .dependsOn(`rho-core`, `rho-swagger`)

lazy val `rho-examples` = project
  .in(file("examples"))
  .disablePlugins(MimaPlugin)
  .settings(buildSettings)
  .settings(
    exampleDeps,
    dontPublish,
    unusedCompileDependenciesFilter -= moduleFilter("org.typelevel", "scalac-compat-annotation")
  )
  .dependsOn(`rho-swagger`, `rho-swagger-ui`)

lazy val disabledCompilerFlags = Seq( // TODO: Fix code and re-enable these.
  "-Xlint:_,-implicit-recursion,-recurse-with-default,-unused,-byname-implicit",
  "-Ywarn-numeric-widen",
  "-Wnumeric-widen",
  "-Yno-adapted-args",
  "-Wdead-code",
  "-Ywarn-dead-code"
)

/* Don't publish setting */
lazy val dontPublish = packagedArtifacts := Map.empty

lazy val buildSettings = publishing ++
  Seq(
    scalacOptions --= disabledCompilerFlags,
    scalacOptions ++= Seq(
      "-Xlint:_,-unused,-byname-implicit,-adapted-args,-package-object-classes"
    ),
    (run / fork) := true,
    description := "A self documenting DSL build upon the http4s framework",
    (ThisBuild / licenses) := Seq(License.Apache2),
    libraryDependencies ++= Seq(
      logbackClassic % "test"
    ),
    libraryDependencies ++= (if (scalaVersion.value.startsWith("2"))
                               Seq(
                                 shapeless,
                                 `scala-reflect` % scalaVersion.value
                               )
                             else Seq.empty),
    libraryDependencies ++= Seq(munit, munitCatsEffect, scalacheckMunit)
  )

// to keep REPL usable
(Compile / console / scalacOptions) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

lazy val publishing = Seq(
  extras,
  // Don't publish root pom.  It's not needed.
  (LocalRootProject / packagedArtifacts) := Map.empty,
  (Test / publishArtifact) := false,
  (ThisBuild / scmInfo) := {
    val base = "github.com/http4s/rho"
    Some(ScmInfo(url(s"https://$base"), s"scm:git:https://$base", Some(s"scm:git:git@$base")))
  }
)

lazy val extras = (ThisBuild / pomExtra) := (
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
    <developer>
      <id>zarthross</id>
      <name>Darren A Gibson</name>
      <email>zarthross@gmail.com</email>
    </developer>
  </developers>
)
