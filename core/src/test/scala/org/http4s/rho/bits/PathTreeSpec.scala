package org.http4s
package rho
package bits

import java.nio.charset.StandardCharsets

import cats.effect.IO
import org.specs2.mutable.Specification
import org.http4s.Uri._
import org.http4s.server.middleware.TranslateUri
import org.http4s.server.Router

class PathTreeSpec extends Specification {
  object pathTree extends PathTreeOps[IO]
  import pathTree._

  def matchNodeFromString(
      name: String,
      matches: Map[String, MatchNode] = Map.empty[String, MatchNode],
      captures: List[CaptureNode] = Nil,
      variadic: Map[Method, Leaf] = Map.empty[Method, Leaf],
      end: Map[Method, Leaf] = Map.empty[Method, Leaf]) = MatchNode(
    Uri.Path.Segment(name),
    matches.map { case (k, v) => Uri.Path.Segment(k) -> v },
    captures,
    variadic,
    end
  )
  def captureNodeFromString(
      parser: StringParser[IO, _],
      matches: Map[String, MatchNode] = Map.empty[String, MatchNode],
      captures: List[CaptureNode] = List.empty[CaptureNode],
      variadic: Map[Method, Leaf] = Map.empty[Method, Leaf],
      end: Map[Method, Leaf] = Map.empty[Method, Leaf]
  ) = CaptureNode(
    parser = parser,
    matches = matches.map { case (k, v) => Uri.Path.Segment(k) -> v },
    captures = captures,
    variadic = variadic,
    end = end
  )

  "Honor UriTranslations" in {
    val svc = TranslateUri("/bar")(Router("/" -> new RhoRoutes[IO] {
      GET / "foo" |>> "foo"
    }.toRoutes())).orNotFound

    val req = Request[IO](Method.GET, uri = uri"/bar/foo")
    val resp = svc(req).unsafeRunSync()

    resp.status must_== Status.Ok
    val b = new String(
      resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _),
      StandardCharsets.UTF_8
    )
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
      svc(req).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status must_== Status.MethodNotAllowed
    }

    "Provide a 404 NotFound when the OPTIONS method is used for a resource without an OPTIONS" in {
      val req = Request[IO](Method.OPTIONS, uri = uri("/foo"))
      svc(req).value.unsafeRunSync().getOrElse(Response.notFound).status must_== Status.NotFound
    }
  }

  "PathTree mergers" >> {
    val l = Leaf((_, _) => null)

    "MatchNodes" should {
      "Merge empty nodes" in {
        val n = matchNodeFromString("")
        n.merge(n) must_== n
      }

      "Merge non-empty nodes with the same method" in {
        val n1 = matchNodeFromString("foo", end = Map(Method.GET -> l))
        val n2 = matchNodeFromString("foo", end = Map(Method.GET -> l))
        n1.merge(n2) must_== n1.copy(end = Map(Method.GET -> (l ++ l)))
      }

      "Merge non-empty nodes with different defined methods" in {
        val n1 = matchNodeFromString("foo", end = Map(Method.GET -> l))
        val n2 = matchNodeFromString("foo", end = Map(Method.POST -> l))
        n1.merge(n2) must_== n1.copy(end = n1.end ++ n2.end)
      }

      "Merge non-empty intermediate nodes with matching paths" in {
        val n1 =
          matchNodeFromString(
            "foo",
            matches = Map("bar" -> matchNodeFromString("bar", end = Map(Method.GET -> l)))
          )
        val n2 =
          matchNodeFromString(
            "foo",
            matches = Map("bar" -> matchNodeFromString("bar", end = Map(Method.POST -> l)))
          )
        val r = matchNodeFromString(
          "foo",
          matches =
            Map("bar" -> matchNodeFromString("bar", end = Map(Method.POST -> l, Method.GET -> l)))
        )

        n1.merge(n2) must_== r
      }

      "Merge non-empty intermediate nodes with non matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = matchNodeFromString("bar", end = endm)
        val bizz = matchNodeFromString("bizz", end = endm)
        val n1 = matchNodeFromString("foo", matches = Map("bar" -> bar))
        val n2 = matchNodeFromString("foo", matches = Map("bizz" -> bizz))

        n1.merge(n2) must_== matchNodeFromString("foo", matches = Map("bar" -> bar, "bizz" -> bizz))
      }

      "Merge non-empty intermediate nodes with mixed matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = matchNodeFromString("bar", end = endm)
        val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = endm)
        val n1 = matchNodeFromString("foo", matches = Map("bar" -> bar))
        val n2 = matchNodeFromString("foo", captures = List(bizz))

        n1.merge(n2) must_== matchNodeFromString(
          "foo",
          matches = Map("bar" -> bar),
          captures = List(bizz)
        )
      }
    }

    "CapturesNodes" should {
      val p = StringParser.booleanParser[IO]
      "Merge empty CaptureNodes" in {
        val n = captureNodeFromString(p)
        n.merge(n) must_== n
      }

      "Merge non-empty nodes with the same method" in {
        val n1 = captureNodeFromString(p, end = Map(Method.GET -> l))
        val n2 = captureNodeFromString(p, end = Map(Method.GET -> l))
        n1.merge(n2) must_== n1.copy(end = Map(Method.GET -> (l ++ l)))
      }

      "Merge non-empty nodes with different defined methods" in {
        val n1 = captureNodeFromString(p, end = Map(Method.GET -> l))
        val n2 = captureNodeFromString(p, end = Map(Method.POST -> l))
        n1.merge(n2) must_== n1.copy(end = n1.end ++ n2.end)
      }

      "Merge non-empty intermediate nodes with matching paths" in {
        val n1 =
          captureNodeFromString(
            p,
            matches = Map("bar" -> matchNodeFromString("bar", end = Map(Method.GET -> l)))
          )
        val n2 =
          captureNodeFromString(
            p,
            matches = Map("bar" -> matchNodeFromString("bar", end = Map(Method.POST -> l)))
          )
        val r = captureNodeFromString(
          p,
          matches =
            Map("bar" -> matchNodeFromString("bar", end = Map(Method.POST -> l, Method.GET -> l)))
        )

        n1.merge(n2) must_== r
      }

      "Merge non-empty intermediate nodes with non matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = matchNodeFromString("bar", end = endm)
        val bizz = matchNodeFromString("bizz", end = endm)
        val n1 = captureNodeFromString(p, matches = Map("bar" -> bar))
        val n2 = captureNodeFromString(p, matches = Map("bizz" -> bizz))

        n1.merge(n2) must_== captureNodeFromString(p, matches = Map("bar" -> bar, "bizz" -> bizz))
      }

      "Merge non-empty intermediate nodes with mixed matching paths" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = matchNodeFromString("bar", end = endm)
        val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = endm)
        val n1 = captureNodeFromString(p, matches = Map("bar" -> bar))
        val n2 = captureNodeFromString(p, captures = List(bizz))

        n1.merge(n2) must_== captureNodeFromString(
          p,
          matches = Map("bar" -> bar),
          captures = List(bizz)
        )
      }

      "Merging should preserve order" in {
        val endm: Map[Method, Leaf] = Map(Method.GET -> l)
        val bar = captureNodeFromString(StringParser.intParser[IO], end = endm)
        val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = endm)
        val n1 = captureNodeFromString(p, captures = List(bar))
        val n2 = captureNodeFromString(p, captures = List(bizz))

        n1.merge(n2) must_== captureNodeFromString(p, captures = List(bar, bizz))
      }

      "Merging should promote order of the same nodes" in {
        val end1: Map[Method, Leaf] = Map(Method.GET -> l)
        val end2: Map[Method, Leaf] = Map(Method.POST -> l)

        val foo = captureNodeFromString(StringParser.shortParser[IO], end = end1)
        val bar = captureNodeFromString(StringParser.intParser[IO], end = end1)
        val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = end1)

        val n1 = captureNodeFromString(p, captures = List(foo, bar))
        val n2 = captureNodeFromString(p, captures = List(bizz, foo.copy(end = end2)))

        n1.merge(n2) must_== captureNodeFromString(
          p,
          captures = List(foo.copy(end = end1 ++ end2), bar, bizz)
        )
      }
    }
  }
}
