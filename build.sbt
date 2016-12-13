import RhoBuild._

version       in ThisBuild := s"0.14.0${scalazCrossBuildSuffix(scalazVersion.value)}-SNAPSHOT"
apiVersion    in ThisBuild <<= version.map(extractApiVersion)
scalazVersion in ThisBuild := "7.1.11"
