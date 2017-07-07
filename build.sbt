import RhoBuild._

version       in ThisBuild := s"0.17.0-M3"
apiVersion    in ThisBuild <<= version.map(extractApiVersion)
