import RhoBuild._

version       in ThisBuild := s"0.10.1${scalazCrossBuildSuffix(scalazVersion.value)}-SNAPSHOT"
apiVersion    in ThisBuild <<= version.map(extractApiVersion)
scalazVersion in ThisBuild := "7.1.7"
