import RhoBuild._
borked! Is travis going to tell me?

version       in ThisBuild := s"0.16.0${scalazCrossBuildSuffix(scalazVersion.value)}-SNAPSHOT"
apiVersion    in ThisBuild <<= version.map(extractApiVersion)
scalazVersion in ThisBuild := "7.1.11"
