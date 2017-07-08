import RhoBuild._

version       in ThisBuild := s"0.17.0-SNAPSHOT"
apiVersion    in ThisBuild <<= version.map(extractApiVersion)
