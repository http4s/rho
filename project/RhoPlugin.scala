import sbt._
import sbt.Keys._
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._
import sbtdynver.DynVerPlugin.autoImport.previousStableVersion

object RhoPlugin extends AutoPlugin {
  object autoImport {
    val apiVersion = taskKey[(Int, Int)]("Defines the API compatibility version for the project.")
  }
  import autoImport._

  override def trigger = allRequirements

  val homepageUrl = "https://github.com/http4s/rho"

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
        val (major, minor) = apiVersion
        s"$homepageUrl/tree/v$major.$minor.0€{FILE_PATH}.scala"
      }

    val opts = List(
      "-implicits",
      "-doc-source-url",
      sourceLoc,
      "-sourcepath",
      base.getAbsolutePath
    )
    opts
  }

  def mimaConfiguration: Seq[Setting[_]] = Seq(
    mimaPreviousArtifacts := previousStableVersion.value
      .map(_ => organization.value %% name.value % "0.21.0-RC2")
      .toSet
      .filter { module =>
        !(module.name == "rho-swagger-ui" && module.revision == "0.20.0")
      }
  )
}
