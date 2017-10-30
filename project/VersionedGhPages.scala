import sbt._
import Keys._

import RhoPlugin.autoImport.apiVersion

import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import com.typesafe.sbt.SbtGit.GitKeys._
import com.typesafe.sbt.git.GitRunner

// Copied from sbt-ghpages to avoid blowing away the old API
// https://github.com/sbt/sbt-ghpages/issues/10
object VersionedGhPages {
  def cleanSiteForRealz(dir: File, git: GitRunner, s: TaskStreams, apiVersion: (Int, Int)): Unit = {
    val toClean = IO.listFiles(dir).collect {
      case f if f.getName == "api" => new java.io.File(f, s"${apiVersion._1}.${apiVersion._2}")
      case f if f.getName != ".git" && f.getName != "CNAME" => f
    }.map(_.getAbsolutePath).toList
    if (toClean.nonEmpty)
      git("rm" :: "-r" :: "-f" :: "--ignore-unmatch" :: toClean :_*)(dir, s.log)
    ()
  }

  def cleanSite0 = Def.task {
    cleanSiteForRealz(updatedRepository.value, gitRunner.value, streams.value, apiVersion.value)
  }

  def synchLocal0 = Def.task {
    val repo = updatedRepository.value
    val apiV@(major, minor) = apiVersion.value
    // TODO - an sbt.Synch with cache of previous mappings to make this more efficient. */
    val betterMappings =  privateMappings.value map { case (file, target) => (file, repo / target) }
    // First, remove 'stale' files.
    cleanSiteForRealz(repo, gitRunner.value, streams.value, apiV)
    // Now copy files.
    IO.copy(betterMappings)
    if(ghpagesNoJekyll.value) IO.touch(repo / ".nojekyll")

    IO.write(repo / "index.html", indexTemplate(major, minor))

    repo
  }

  def indexTemplate(major: Int, minor: Int) = s"""
      |<!DOCTYPE html>
      |<html lang="en">
      |  <head>
      |    <meta charset="UTF-8">
      |    <title>Project Documentation</title>
      |    <script language="JavaScript">
      |    <!--
      |    function doRedirect() {
      |     // redirect to the latest docs
      |      window.location.replace("api/$major.$minor");
      |    }
      |
      |    doRedirect();
      |    //-->
      |    </script>
      |  </head>
      |  <body>
      |    <a href="api/$major.$minor">Go to the project documentation</a>
      |  </body>
      |</html>
    """.stripMargin
}
