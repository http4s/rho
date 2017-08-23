import RhoBuild._

version       in ThisBuild := s"0.17.0-RC1"
apiVersion    in ThisBuild <<= version.map(extractApiVersion)
