import sbt._
import Keys._
import spray.revolver.RevolverPlugin._

import com.typesafe.sbt.SbtGit.git

import Dependencies._, RhoPlugin._

lazy val rho = project
  .in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(buildSettings: _*)
  .aggregate(`rho-core`, `rho-swagger`, `rho-swagger-ui`, `rho-examples`)

lazy val `rho-core` = project
  .in(file("core"))
  .settings(mimaConfiguration)
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
    libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0")
  )

lazy val `rho-swagger` = project
  .in(file("swagger"))
  .settings(buildSettings :+ swaggerDeps: _*)
  .settings(mimaConfiguration)
  .dependsOn(`rho-core` % "compile->compile;test->test")

lazy val `rho-swagger-ui` = project
  .in(file("swagger-ui"))
  .settings(buildSettings :+ swaggerUiDeps: _*)
  .settings(mimaConfiguration)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    dontPublish,
    buildInfoKeys := Seq[BuildInfoKey]("swaggerUiVersion" -> Dependencies.swaggerUi.revision),
    buildInfoPackage := "org.http4s.rho.swagger.ui"
  )
  .dependsOn(`rho-swagger`)

lazy val docs = project
  .in(file("docs"))
  .settings(buildSettings)
  .disablePlugins(MimaPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(SiteScaladocPlugin)
  .enablePlugins(GhpagesPlugin)
  .settings(
    dontPublish,
    description := "Api Documentation",
    autoAPIMappings := true,
    (Compile / scalacOptions) := scaladocOptions(
      (ThisBuild / baseDirectory).value,
      version.value,
      apiVersion.value
    ),
    (ScalaUnidoc / unidoc / unidocProjectFilter) := inProjects(
      `rho-core`,
      `rho-swagger`
    ),
    git.remoteRepo := "git@github.com:http4s/rho.git",
    ghpagesCleanSite := VersionedGhPages.cleanSite0.value,
    ghpagesSynchLocal := VersionedGhPages.synchLocal0.value,
    (makeSite / mappings) := {
      val (major, minor) = apiVersion.value
      for {
        (f, d) <- (ScalaUnidoc / packageDoc / mappings).value
      } yield (f, s"api/$major.$minor/$d")
    }
  )
  .dependsOn(`rho-core`, `rho-swagger`)

lazy val `rho-examples` = project
  .in(file("examples"))
  .disablePlugins(MimaPlugin)
  .settings(buildSettings)
  .settings(Revolver.settings)
  .settings(
    exampleDeps,
    dontPublish
  )
  .dependsOn(`rho-swagger`, `rho-swagger-ui`)

lazy val disabledCompilerFlags = Seq( // TODO: Fix code and re-enable these.
  "-Xlint:package-object-classes",
  "-Ywarn-numeric-widen",
  "-Wnumeric-widen",
  "-Xlint:adapted-args",
  "-Yno-adapted-args",
  "-Wdead-code",
  "-Ywarn-dead-code"
)

/* Don't publish setting */
lazy val dontPublish = packagedArtifacts := Map.empty

lazy val license = (ThisBuild / licenses) := Seq(
  "Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")
)

lazy val buildSettings = publishing ++
  Seq(
    scalaVersion := scala_213,
    crossScalaVersions := Seq(scala_213, scala_212),
    scalacOptions --= disabledCompilerFlags,
    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      "Residenthome Private" at "https://resident.jfrog.io/artifactory/private/"
    ),
    (run / fork) := true,
    (ThisBuild / organization) := "com.inspiredme",
    (ThisBuild / homepage) := Some(url(homepageUrl)),
    description := "A self documenting DSL build upon the http4s framework",
    license,
    libraryDependencies ++= Seq(
      http4sServer % "provided",
      logbackClassic % "test"
    ),
    libraryDependencies ++= (if (scalaVersion.value.startsWith("2"))
                               Seq(
                                 shapeless,
                                 silencerPlugin,
                                 silencerLib,
                                 kindProjector,
                                 `scala-reflect` % scalaVersion.value
                               )
                             else Seq.empty),
    libraryDependencies ++= Seq(munit, munitCatsEffect, scalacheckMunit),
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
    publishTo := {
      val artifactory = "https://resident.jfrog.io/artifactory/"
        Some("Residenthome Releases" at artifactory + "releases;build.commit_number=" +
          sys.env.getOrElse("CIRCLE_SHA1", "unknown") +
          ";build.username=" +
          sys.env.getOrElse("CIRCLE_USERNAME", "unknown") + ";")
    }
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
  },
  packageDoc / publishArtifact := false,
  packageSrc / publishArtifact := true
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
