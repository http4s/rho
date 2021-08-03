package org.http4s
package rho
package bits

import java.nio.charset.StandardCharsets

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.server.middleware.TranslateUri
import org.http4s.server.Router

class PathTreeSuite extends CatsEffectSuite {
  object pathTree extends PathTreeOps[IO]
  import pathTree._

  private def matchNodeFromString(
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

  private def captureNodeFromString(
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

  test("Honor UriTranslations") {
    val svc = TranslateUri("/bar")(Router("/" -> new RhoRoutes[IO] {
      GET / "foo" |>> "foo"
    }.toRoutes())).orNotFound

    val req = Request[IO](Method.GET, uri = uri"/bar/foo")

    for {
      resp <- svc(req)
      _ = assertEquals(resp.status, Status.Ok)
      b <- resp.body.compile.toVector.map(_.foldLeft(Array[Byte]())(_ :+ _))
      _ = assertEquals(new String(b, StandardCharsets.UTF_8), "foo")
    } yield ()
  }

  private val svc = new RhoRoutes[IO] {
    GET / "foo" |>> "foo"

    OPTIONS / "bar" |>> "foo"
  }.toRoutes()

  test("PathTree OPTIONS should handle a valid OPTIONS request") {
    val req = Request[IO](Method.OPTIONS, uri = uri"/bar")
    assertIO(svc(req).value.map(_.getOrElse(Response.notFound).status), Status.Ok)
  }

  test(
    "PathTree OPTIONS should provide a 405 MethodNotAllowed when an incorrect method is used for a resource"
  ) {
    val req = Request[IO](Method.POST, uri = uri"/foo")
    assertIO(
      svc(req).value
        .map(_.getOrElse(Response.notFound).status),
      Status.MethodNotAllowed
    )
  }

  test(
    "PathTree OPTIONS should provide a 404 NotFound when the OPTIONS method is used for a resource without an OPTIONS"
  ) {
    val req = Request[IO](Method.OPTIONS, uri = uri"/foo")
    assertIO(
      svc(req).value.map(_.getOrElse(Response.notFound).status),
      Status.NotFound
    )
  }

  private val l = Leaf((_, _) => null)

  test("PathTree mergers. MatchNodes should merge empty nodes") {
    val n = matchNodeFromString("")
    assertEquals(n.merge(n), n)
  }

  test("PathTree mergers. MatchNodes should merge non-empty nodes with the same method") {
    val n1 = matchNodeFromString("foo", end = Map(Method.GET -> l))
    val n2 = matchNodeFromString("foo", end = Map(Method.GET -> l))
    assertEquals(n1.merge(n2), n1.copy(end = Map(Method.GET -> (l ++ l))))
  }

  test("PathTree mergers. MatchNodes should merge non-empty nodes with different defined methods") {
    val n1 = matchNodeFromString("foo", end = Map(Method.GET -> l))
    val n2 = matchNodeFromString("foo", end = Map(Method.POST -> l))
    assertEquals(n1.merge(n2), n1.copy(end = n1.end ++ n2.end))
  }

  test(
    "PathTree mergers. MatchNodes should merge non-empty intermediate nodes with matching paths"
  ) {
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

    assertEquals(n1.merge(n2), r)
  }

  test(
    "PathTree mergers. MatchNodes should merge non-empty intermediate nodes with non matching paths"
  ) {
    val endm: Map[Method, Leaf] = Map(Method.GET -> l)
    val bar = matchNodeFromString("bar", end = endm)
    val bizz = matchNodeFromString("bizz", end = endm)
    val n1 = matchNodeFromString("foo", matches = Map("bar" -> bar))
    val n2 = matchNodeFromString("foo", matches = Map("bizz" -> bizz))

    assertEquals(
      n1.merge(n2),
      matchNodeFromString("foo", matches = Map("bar" -> bar, "bizz" -> bizz))
    )
  }

  test(
    "PathTree mergers. MatchNodes should merge non-empty intermediate nodes with mixed matching paths"
  ) {
    val endm: Map[Method, Leaf] = Map(Method.GET -> l)
    val bar = matchNodeFromString("bar", end = endm)
    val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = endm)
    val n1 = matchNodeFromString("foo", matches = Map("bar" -> bar))
    val n2 = matchNodeFromString("foo", captures = List(bizz))

    assertEquals(
      n1.merge(n2),
      matchNodeFromString(
        "foo",
        matches = Map("bar" -> bar),
        captures = List(bizz)
      )
    )
  }

  private val p = StringParser.booleanParser[IO]

  test("PathTree mergers. CapturesNodes should merge empty CaptureNodes") {
    val n = captureNodeFromString(p)
    assertEquals(n.merge(n), n)
  }

  test("PathTree mergers. CapturesNodes should merge non-empty nodes with the same method") {
    val n1 = captureNodeFromString(p, end = Map(Method.GET -> l))
    val n2 = captureNodeFromString(p, end = Map(Method.GET -> l))
    assertEquals(n1.merge(n2), n1.copy(end = Map(Method.GET -> (l ++ l))))
  }

  test(
    "PathTree mergers. CapturesNodes should merge non-empty nodes with different defined methods"
  ) {
    val n1 = captureNodeFromString(p, end = Map(Method.GET -> l))
    val n2 = captureNodeFromString(p, end = Map(Method.POST -> l))
    assertEquals(n1.merge(n2), n1.copy(end = n1.end ++ n2.end))
  }

  test(
    "PathTree mergers. CapturesNodes should merge non-empty intermediate nodes with matching paths"
  ) {
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

    assertEquals(n1.merge(n2), r)
  }

  test(
    "PathTree mergers. CapturesNodes should merge non-empty intermediate nodes with non matching paths"
  ) {
    val endm: Map[Method, Leaf] = Map(Method.GET -> l)
    val bar = matchNodeFromString("bar", end = endm)
    val bizz = matchNodeFromString("bizz", end = endm)
    val n1 = captureNodeFromString(p, matches = Map("bar" -> bar))
    val n2 = captureNodeFromString(p, matches = Map("bizz" -> bizz))

    assertEquals(
      n1.merge(n2),
      captureNodeFromString(p, matches = Map("bar" -> bar, "bizz" -> bizz))
    )
  }

  test(
    "PathTree mergers. CapturesNodes should merge non-empty intermediate nodes with mixed matching paths"
  ) {
    val endm: Map[Method, Leaf] = Map(Method.GET -> l)
    val bar = matchNodeFromString("bar", end = endm)
    val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = endm)
    val n1 = captureNodeFromString(p, matches = Map("bar" -> bar))
    val n2 = captureNodeFromString(p, captures = List(bizz))

    assertEquals(
      n1.merge(n2),
      captureNodeFromString(
        p,
        matches = Map("bar" -> bar),
        captures = List(bizz)
      )
    )
  }

  test("PathTree mergers. CapturesNodes merging should preserve order") {
    val endm: Map[Method, Leaf] = Map(Method.GET -> l)
    val bar = captureNodeFromString(StringParser.intParser[IO], end = endm)
    val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = endm)
    val n1 = captureNodeFromString(p, captures = List(bar))
    val n2 = captureNodeFromString(p, captures = List(bizz))

    assertEquals(n1.merge(n2), captureNodeFromString(p, captures = List(bar, bizz)))
  }

  test("PathTree mergers. CapturesNodes merging should promote order of the same nodes") {
    val end1: Map[Method, Leaf] = Map(Method.GET -> l)
    val end2: Map[Method, Leaf] = Map(Method.POST -> l)

    val foo = captureNodeFromString(StringParser.shortParser[IO], end = end1)
    val bar = captureNodeFromString(StringParser.intParser[IO], end = end1)
    val bizz = captureNodeFromString(StringParser.booleanParser[IO], end = end1)

    val n1 = captureNodeFromString(p, captures = List(foo, bar))
    val n2 = captureNodeFromString(p, captures = List(bizz, foo.copy(end = end2)))

    assertEquals(
      n1.merge(n2),
      captureNodeFromString(
        p,
        captures = List(foo.copy(end = end1 ++ end2), bar, bizz)
      )
    )
  }
}
