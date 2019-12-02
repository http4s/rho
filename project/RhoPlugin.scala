import sbt._
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport.{mimaFailOnNoPrevious, mimaFailOnProblem, mimaPreviousArtifacts}
import sbt.Keys.{moduleName, organization, scalaBinaryVersion, version}

object RhoPlugin extends AutoPlugin {
  object autoImport {
    val apiVersion = taskKey[(Int, Int)]("Defines the API compatibility version for the project.")
    val http4sMimaVersion = settingKey[Option[String]]("Version to target for MiMa compatibility")
  }
  import autoImport._

  override def trigger = allRequirements

  val homepageUrl = "https://github.com/http4s/rho"

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

  def mimaConfiguration: Seq[Setting[_]] = Seq(
    http4sMimaVersion := {
      version.value match {
        case VersionNumber(Seq(major, minor, patch), _, _) if patch.toInt > 0 =>
          Some(s"$major.$minor.${patch.toInt - 1}")
        case _ =>
          None
      }
    },
    mimaFailOnProblem := http4sMimaVersion.value.isDefined,
    mimaFailOnNoPrevious := false,
    mimaPreviousArtifacts := (http4sMimaVersion.value.map {
      organization.value % s"${moduleName.value}_${scalaBinaryVersion.value}" % _
    }).toSet
  )
}
