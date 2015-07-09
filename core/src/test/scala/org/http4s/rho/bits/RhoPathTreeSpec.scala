package org.http4s.rho.bits

import java.nio.charset.StandardCharsets

import org.http4s.rho._
import org.http4s._
import org.http4s.rho.RhoService

import org.specs2.mutable.Specification

import org.http4s.server.middleware.URITranslation

class RhoPathTreeSpec extends Specification {
  import PathTree._

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
    "Not swallow a trailing '/'" in {
      splitPath("/test/") must_== List("test", "")
    }
  }

  "Honor UriTranslations" in {
    val svc = URITranslation.translateRoot("/bar")(new RhoService {
      GET / "foo" |>> AAction{ "foo" }
    }.toService)
    val req = Request(Method.GET, uri = Uri(path = "/bar/foo"))
    val resp = svc(req).run.get

    resp.status must_== Status.Ok
    val b = new String(resp.body.runLog.run.reduce(_ ++ _).toArray, StandardCharsets.UTF_8)
    b must_== "foo"
  }
}
