package org.http4s.rho.bits

import org.specs2.mutable.Specification

class RhoPathTreeSpec extends Specification {
  import RhoPathTree._

  "splitPath" should {
    "handle an empty string" in {
      splitPath("") must_== List("")
    }
    "handle '/' only" in {
      splitPath("/") must_== List("")
    }
    "handle /test/path" in {
      splitPath("/test/path") must_== List("test", "path")
    }
    "Interpret '//' as '/'" in {
      splitPath("/test//path") must_== List("test", "path")
    }
  }
}
