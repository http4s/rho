package org.http4s
package rho
package bits

import java.nio.charset.StandardCharsets

import cats.effect.IO
import org.http4s.server.Router
import org.http4s.server.middleware.TranslateUri
import org.specs2.mutable.Specification

class PathTreeSpec extends Specification {
  import PathTree._
  object pathTree extends PathTreeOps[IO]
  import pathTree._

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
    "Url Decode Path segments" in {
      splitPath("/test/%23%24%25!%40/foobar") must_== List("test", "#$%!@", "foobar")
    }
  }

  "Honor UriTranslations" in {
    val svc = TranslateUri("/bar")(Router.define[IO](("/", new RhoRoutes[IO] {
      GET / "foo" |>> "foo"
    }.toRoutes()))(HttpRoutes.empty[IO]))

    val req = Request[IO](Method.GET, uri = Uri(path = "/bar/foo"))
    val resp = svc(req).value.unsafeRunSync().getOrElse(Response.notFound)

    resp.status must_== Status.Ok
    val b = new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _), StandardCharsets.UTF_8)
    b must_== "foo"
  }

  "PathTree OPTIONS" should {
    val svc = new RhoRoutes[IO] {
      GET / "foo" |>> "foo"

      OPTIONS / "bar" |>> "foo"
    }.toRoutes()

    "Handle a valid OPTIONS request" in {
      val req = Request[IO](Method.OPTIONS, uri = uri("/bar"))
      svc(req).value.unsafeRunSync().getOrElse(Response.notFound).status must_== Status.Ok
    }

    "Provide a 405 MethodNotAllowed when an incorrect method is used for a resource" in {
      val req = Request[IO](Method.POST, uri = uri("/foo"))
      svc(req).value.unsafeRunSync().getOrElse(Response.notFound).status must_== Status.MethodNotAllowed
    }

    "Provide a 404 NotFound when the OPTIONS method is used for a resource without an OPTIONS" in {
      val req = Request[IO](Method.OPTIONS, uri = uri("/foo"))
      svc(req).value.unsafeRunSync().getOrElse(Response.notFound).status must_== Status.NotFound
    }
  }

  "PathTree mergers" >> {
    val l = Leaf((r, b) => null)

    "MatchNodes" should {
      "Merge empty nodes" in {
        val n = MatchNode("")
        n merge n must_== n
      }

      "Merge non-empty nodes with the same method" in {
        val n1 = MatchNode("foo", end = Map(Method.GET -> l))
        val n2 = MatchNode("foo", end = Map(Method.GET -> l))
        n1 merge n2 must_== n1.copy(end = Map(Method.GET -> (l ++ l)))
      }

      "Merge non-empty nodes with different defined methods" in {
        val n1 = MatchNode("foo", end = Map(Method.GET -> l))
        val n2 = MatchNode("foo", end = Map(Method.POST -> l))
        n1 merge n2 must_== n1.copy(end = n1.end ++ n2.end)
      }

      "Merge non-empty intermediate nodes with matching paths" in {
        val n1 = MatchNode("foo", matches = Map("bar" -> MatchNode("bar", end = Map(Method.GET -> l))))
        val n2 = MatchNode("foo", matches = Map("bar" -> MatchNode("bar", end = Map(Method.POST -> l))))
        val r = MatchNode("foo", matches = Map("bar" -> MatchNode("bar", end = Map(Method.POST -> l, Method.GET -> l))))

        n1 merge n2 must_== r
      }

      "Merge non-empty intermediate nodes with non matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = MatchNode("bar", end = endm)
        val bizz = MatchNode("bizz", end = endm)
        val n1 = MatchNode("foo", matches = Map("bar" -> bar))
        val n2 = MatchNode("foo", matches = Map("bizz" -> bizz))

        n1 merge n2 must_== MatchNode("foo", matches = Map("bar" -> bar, "bizz" -> bizz))
      }

      "Merge non-empty intermediate nodes with mixed matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = MatchNode("bar", end = endm)
        val bizz = CaptureNode(StringParser.booleanParser[IO], end = endm)
        val n1 = MatchNode("foo", matches = Map("bar" -> bar))
        val n2 = MatchNode("foo", captures = List(bizz))

        n1 merge n2 must_== MatchNode("foo", matches = Map("bar" -> bar), captures = List(bizz))
      }
    }

    "CapturesNodes" should {
      val p = StringParser.booleanParser[IO]
      "Merge empty CaptureNodes" in {
        val n = CaptureNode(p)
        n merge n must_== n
      }

      "Merge non-empty nodes with the same method" in {
        val n1 = CaptureNode(p, end = Map(Method.GET -> l))
        val n2 = CaptureNode(p, end = Map(Method.GET -> l))
        n1 merge n2 must_== n1.copy(end = Map(Method.GET -> (l ++ l)))
      }

      "Merge non-empty nodes with different defined methods" in {
        val n1 = CaptureNode(p, end = Map(Method.GET -> l))
        val n2 = CaptureNode(p, end = Map(Method.POST -> l))
        n1 merge n2 must_== n1.copy(end = n1.end ++ n2.end)
      }

      "Merge non-empty intermediate nodes with matching paths" in {
        val n1 = CaptureNode(p, matches = Map("bar" -> MatchNode("bar", end = Map(Method.GET -> l))))
        val n2 = CaptureNode(p, matches = Map("bar" -> MatchNode("bar", end = Map(Method.POST -> l))))
        val r = CaptureNode(p, matches = Map("bar" -> MatchNode("bar", end = Map(Method.POST -> l, Method.GET -> l))))

        n1 merge n2 must_== r
      }

      "Merge non-empty intermediate nodes with non matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = MatchNode("bar", end = endm)
        val bizz = MatchNode("bizz", end = endm)
        val n1 = CaptureNode(p, matches = Map("bar" -> bar))
        val n2 = CaptureNode(p, matches = Map("bizz" -> bizz))

        n1 merge n2 must_== CaptureNode(p, matches = Map("bar" -> bar, "bizz" -> bizz))
      }

      "Merge non-empty intermediate nodes with mixed matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = MatchNode("bar", end = endm)
        val bizz = CaptureNode(StringParser.booleanParser[IO], end = endm)
        val n1 = CaptureNode(p, matches = Map("bar" -> bar))
        val n2 = CaptureNode(p, captures = List(bizz))

        n1 merge n2 must_== CaptureNode(p, matches = Map("bar" -> bar), captures = List(bizz))
      }

      "Merging should preserve order" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = CaptureNode(StringParser.intParser[IO], end = endm)
        val bizz = CaptureNode(StringParser.booleanParser[IO], end = endm)
        val n1 = CaptureNode(p, captures = List(bar))
        val n2 = CaptureNode(p, captures = List(bizz))

        n1 merge n2 must_== CaptureNode(p, captures = List(bar, bizz))
      }

      "Merging should promote order of the same nodes" in {
        val end1: Map[Method, Leaf] = Map(Method.GET -> l)
        val end2: Map[Method, Leaf] = Map(Method.POST -> l)

        val foo = CaptureNode(StringParser.shortParser[IO], end = end1)
        val bar = CaptureNode(StringParser.intParser[IO], end = end1)
        val bizz = CaptureNode(StringParser.booleanParser[IO], end = end1)

        val n1 = CaptureNode(p, captures = List(foo, bar))
        val n2 = CaptureNode(p, captures = List(bizz, foo.copy(end = end2)))

        n1 merge n2 must_== CaptureNode(p, captures = List(foo.copy(end = end1 ++ end2), bar, bizz))
      }
    }
  }
}
