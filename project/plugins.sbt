resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.2")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

addSbtPlugin("ch.epfl.scala" % "sbt-release-early" % "2.1.1")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.3.0")