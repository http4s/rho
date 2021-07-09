package org.http4s
package rho

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import munit.FunSuite
import org.http4s.headers.{`Content-Length`, `Content-Type`}
import org.http4s.rho.io._
import org.http4s.Uri.Path
import org.typelevel.ci.CIString

import scala.collection.compat.immutable.ArraySeq
import scala.collection.immutable.Seq
import scala.util.control.NoStackTrace

class RhoRoutesSuite extends FunSuite with RequestRunner {
  def construct(method: Method, s: String, h: Header.ToRaw*): Request[IO] =
    Request(method, Uri.fromString(s).getOrElse(sys.error("Failed.")), headers = Headers(h: _*))

  def Get(s: String, h: Header.ToRaw*): Request[IO] = construct(Method.GET, s, h: _*)
  def Put(s: String, h: Header.ToRaw*): Request[IO] = construct(Method.PUT, s, h: _*)

  val httpRoutes = new RhoRoutes[IO] {
    GET +? param("foo", "bar") |>> { foo: String => Ok(s"just root with parameter 'foo=$foo'") }

    GET / "" +? param("foo", "bar") |>> { _: String =>
      Ok("this definition should be hidden by the previous definition")
    }

    GET / "hello" |>> Ok("route1")

    GET / pv"hello" |>> { _: String => Ok("route2") }

    GET / "hello" / "world" |>> Ok("route3")

    GET / "hello/world2" |>> Ok("/hello/world2")

    GET / "hello" / "headers" +? param[Int]("foo") |>> { foo: Int => Ok("route" + foo) }

    GET / "hello" / "default" / "parameter" +? param[Int]("some", 23) |>> { s: Int =>
      Ok("some:" + s)
    }

    // Routes that will have different headers/query string requirements should work together
    GET / "hello" / "compete" +? param[Int]("foo") |>> { foo: Int => Ok("route" + foo) }

    GET / "hello" / "compete" +? param[String]("foo") |>> { foo: String => Ok("route6_" + foo) }

    GET / "hello" / "compete" |>> Ok("route7")

    GET / "hello" / "decoded" / pathVar[String]("v") |>> { foo: String => Ok("route7.5| " + foo) }

    // Testing query params
    GET / "query" / "twoparams" +? (param[Int]("foo") and param[String]("bar")) |>> {
      (foo: Int, bar: String) =>
        Ok("twoparams" + foo + bar)
    }

    GET / "query" / "twoparams2" +? param[Int]("foo") & param[Option[String]]("bar") |>> {
      (foo: Int, bar: Option[String]) =>
        Ok("twoparams2_" + foo + bar.getOrElse("cat"))
    }

    GET / "variadic" / * |>> { tail: List[String] => Ok("route8_" + tail.mkString("/")) }

    val or = "or1" || "or2"
    GET / or |>> { () => Ok("route9") }

    GET / "orders" / pathVar[Int]("id") |>> { id: Int => Ok(id.toString) }

    GET / "options" +? param[Option[String]]("foo") |>> { os: Option[String] =>
      Ok(os.getOrElse("None"))
    }

    GET / "seq" +? param[Seq[String]]("foo") |>> { os: Seq[String] => Ok(os.mkString(" ")) }

    GET / "seq" +? param[Seq[Int]]("foo") |>> { os: Seq[Int] => Ok(os.mkString(" ")) }

    GET / "withreq" +? param[String]("foo") |>> { (_: Request[IO], foo: String) => Ok(s"req $foo") }

    val rootSome = root / "some"
    GET / rootSome |>> Ok("root to some")

    GET / "directTask" |>> {
      val i = new AtomicInteger(0)
      IO(s"${i.getAndIncrement}")
    }

    GET / "terminal" / "" |>> "terminal/"

    GET / "terminal" |>> "terminal"

    GET / "one" / "two" / "three" |>> "one/two"
  }.toRoutes()

  test("A RhoRoutes execution should handle definition without a path, which points to '/'") {
    val request = Request[IO](Method.GET, uri"/")
    assertEquals(checkOk(request), "just root with parameter 'foo=bar'")
  }

  test("A RhoRoutes execution should handle definition without a path but with a parameter") {
    val request = Request[IO](Method.GET, uri"/?foo=biz")
    assertEquals(checkOk(request), "just root with parameter 'foo=biz'")
  }

  test(
    "A RhoRoutes execution should return a 405 when a path is defined but the method doesn't match"
  ) {
    val request = Request[IO](Method.POST, uri"/hello")
    val resp = httpRoutes(request).value.unsafeRunSync().getOrElse(Response.notFound)

    assertEquals(resp.status, Status.MethodNotAllowed)
    assert(
      resp.headers
        .get(CIString("Allow"))
        .map(_.toList)
        .toList
        .flatten
        .contains(
          Header.Raw(CIString("Allow"), "GET")
        )
    )
  }

  test("A RhoRoutes execution should yield `MethodNotAllowed` when invalid method used") {
    assertEquals(
      httpRoutes(Put("/one/two/three")).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status,
      Status.MethodNotAllowed
    )
  }

  test("A RhoRoutes execution should consider PathMatch(\"\") a NOOP") {
    val service = new RhoRoutes[IO] {
      GET / "" / "foo" |>> Ok("bar")
    }.toRoutes()

    val req1 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")))
    assertEquals(
      getBody(service(req1).value.unsafeRunSync().getOrElse(Response.notFound).body),
      "bar"
    )

    val req2 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("//foo")))
    assertEquals(
      getBody(service(req2).value.unsafeRunSync().getOrElse(Response.notFound).body),
      "bar"
    )
  }

  test("A RhoRoutes execution should execute a route with no params") {
    val req = Get("/hello")
    assertEquals(checkOk(req), "route1")
  }

  test("A RhoRoutes execution should execute a route with a single param") {
    val req = Get("/world")
    assertEquals(checkOk(req), "route2")
  }

  test("A RhoRoutes execution should execute a route with concatonated path") {
    val req = Get("/hello/world2")
    assertEquals(checkOk(req), "/hello/world2")
  }

  test("A RhoRoutes execution should execute a route with a single param") {
    val req = Get("/hello/world")
    assertEquals(checkOk(req), "route3")
  }

  test("A RhoRoutes execution should execute a route with a single url encoded param") {
    val req = Get("/hello/decoded/fa%23%24%25!%40sd")
    assertEquals(checkOk(req), "route7.5| fa#$%!@sd")
  }

  test("A RhoRoutes execution should execute a route with a query") {
    val req = Get("/hello/headers?foo=4")
    assertEquals(checkOk(req), "route4")
  }

  test("A RhoRoutes execution should NotFound on empty route") {
    assertEquals(
      httpRoutes(Get("/one/two")).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status,
      Status.NotFound
    )
  }

  test("A RhoRoutes execution should fail a route with a missing query") {
    val req = Get("/hello/headers")
    assertEquals(checkError(req), "Missing query param: foo")
  }

  test("A RhoRoutes execution should fail a route with an invalid query") {
    val req = Get("/hello/headers?foo=bar")
    assertEquals(checkError(req), "Invalid number format: 'bar'")
  }

  test("A RhoRoutes execution should execute a route with multiple query with parameters") {
    "query" / "twoparams"
    val req = Get("/query/twoparams?foo=5&bar=cat")
    assertEquals(checkOk(req), "twoparams5cat")
  }

  test(
    "A RhoRoutes execution should execute a route with multiple query with parameters, one optional"
  ) {
    "query" / "twoparams"
    val req = Get("/query/twoparams2?foo=5")
    assertEquals(checkOk(req), "twoparams2_5cat")
  }

  test("A RhoRoutes execution should execute a route with a query to override parameter default") {
    val req = Get("/hello/default/parameter?some=42")
    assertEquals(checkOk(req), "some:42")
  }

  test("A RhoRoutes execution should execute a route with a query with default parameter") {
    val req = Get("/hello/default/parameter")
    assertEquals(checkOk(req), "some:23")
  }

  test("A RhoRoutes execution should fail a route with an invalid parameter type") {
    val req = Get("/hello/default/parameter?some=a")
    assertEquals(checkError(req), "Invalid number format: 'a'")
  }

  test("A RhoRoutes execution should execute a route with a competing query") {
    val req1 = Get("/hello/compete?foo=5")
    val req2 = Get("/hello/compete?foo=bar")
    val req3 = Get("/hello/compete")

    assertEquals(checkOk(req1), "route5")
    assertEquals(checkOk(req2), "route6_bar")
    assertEquals(checkOk(req3), "route7")
  }

  test("A RhoRoutes execution should execute a variadic route") {
    val req1 = Get("/variadic")
    val req2 = Get("/variadic/one")
    val req3 = Get("/variadic/one/two")

    assertEquals(checkOk(req1), "route8_")
    assertEquals(checkOk(req2), "route8_one")
    assertEquals(checkOk(req3), "route8_one/two")
  }

  test("A RhoRoutes execution should perform path 'or' logic") {
    val req1 = Get("/or1")
    val req2 = Get("/or2")
    assertEquals(checkOk(req1), "route9")
    assertEquals(checkOk(req2), "route9")
  }

  test("A RhoRoutes execution should work with options") {
    val req1 = Get("/options")
    val req2 = Get("/options?foo=bar")
    assertEquals(checkOk(req1), "None")
    assertEquals(checkOk(req2), "bar")
  }

  test("A RhoRoutes execution should work with collections") {
    val req1 = Get("/seq")
    val req2 = Get("/seq?foo=bar")
    val req3 = Get("/seq?foo=1&foo=2")
    assertEquals(checkOk(req1), "")
    assertEquals(checkOk(req2), "bar")
    assertEquals(checkOk(req3), "1 2")
  }

  test("A RhoRoutes execution should provide the request if desired") {
    val req = Get("/withreq?foo=bar")
    assertEquals(checkOk(req), "req bar")
  }

  test("A RhoRoutes execution should level one path definition to /some") {
    val req1 = Request[IO](Method.GET, uri"/some")
    assertEquals(checkOk(req1), "root to some")
  }

  test("A RhoRoutes execution should execute a directly provided Task every invocation") {
    val req = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("directTask")))
    assertEquals(checkOk(req), "0")
    assertEquals(checkOk(req), "1")
  }

  test("A RhoRoutes execution should interpret uris ending in '/' differently than those without") {
    val req1 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("terminal/")))
    assertEquals(checkOk(req1), "terminal/")

    val req2 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("terminal")))
    assertEquals(checkOk(req2), "terminal")
  }

  ///// Order of execution tests /////////////////////
  test("A RhoRoutes execution should attempt to evaluate params in order") {
    val service = new RhoRoutes[IO] {
      GET / "foo" +? param[Int]("bar") |>> { i: Int => Ok(s"Int: $i") }
      GET / "foo" +? param[String]("bar") |>> { i: String => Ok(s"String: $i") }
      GET / "foo" |>> Ok("none")
    }.toRoutes()

    val req1 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")).+?("bar", "0"))
    assertEquals(
      getBody(
        service(req1).value.unsafeRunSync().getOrElse(Response.notFound).body
      ),
      "Int: 0"
    )

    val req2 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")).+?("bar", "s"))
    assertEquals(
      getBody(
        service(req2).value.unsafeRunSync().getOrElse(Response.notFound).body
      ),
      "String: s"
    )

    val req3 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")))
    assertEquals(
      getBody(service(req3).value.unsafeRunSync().getOrElse(Response.notFound).body),
      "none"
    )
  }

  test(
    "A RhoRoutes execution should fail to match more specific routes defined after a less specific route"
  ) {
    val service = new RhoRoutes[IO] {
      GET / "foo" +? param[String]("bar") |>> { i: String => Ok(s"String: $i") }
      GET / "foo" +? param[Int]("bar") |>> { i: Int => Ok(s"Int: $i") }
    }.toRoutes()

    val req1 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")).+?("bar", "0"))
    assertEquals(
      getBody(
        service(req1).value.unsafeRunSync().getOrElse(Response.notFound).body
      ),
      "String: 0"
    )

    val req2 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")).+?("bar", "s"))
    assertEquals(
      getBody(
        service(req2).value.unsafeRunSync().getOrElse(Response.notFound).body
      ),
      "String: s"
    )
  }

  test("A RhoRoutes execution should match an empty Option over a bare route") {
    val service = new RhoRoutes[IO] {
      GET / "foo" +? param[Option[String]]("bar") |>> { o: Option[String] =>
        o.map(s => s"String: $s").getOrElse("none")
      }

      GET / "foo" |>> Ok(s"failure")
    }.toRoutes()

    val req1 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")).+?("bar", "s"))
    assertEquals(
      getBody(
        service(req1).value.unsafeRunSync().getOrElse(Response.notFound).body
      ),
      "String: s"
    )

    val req2 = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/foo")))
    assertEquals(
      getBody(service(req2).value.unsafeRunSync().getOrElse(Response.notFound).body),
      "none"
    )
  }

  test("A RhoRoutes execution should work with all syntax elements") {
    val reqHeader = H[`Content-Length`].existsAnd(h => h.length <= 3)

    val srvc = new RhoRoutes[IO] {
      POST / "foo" / pathVar[Int] +? param[String]("param") >>> reqHeader ^ EntityDecoder
        .text[IO] |>> { (_: Int, _: String, _: String) =>
        Ok("success")
      }
    }

    val body = Stream.emits(ArraySeq.unsafeWrapArray("foo".getBytes()))
    val uri = Uri.fromString("/foo/1?param=myparam").getOrElse(sys.error("Failed."))
    val req = Request[IO](method = Method.POST, uri = uri, body = body)
      .putHeaders(
        `Content-Type`(MediaType.text.plain),
        `Content-Length`.unsafeFromLong("foo".length)
      )

    val r = srvc.toRoutes()(req)
    assertEquals(getBody(r.value.unsafeRunSync().getOrElse(Response.notFound).body), "success")
  }

  ////////////////////////////////////////////////////
  test("A RhoRoutes execution should handle errors in the route actions") {
    val service = new RhoRoutes[IO] {
      GET / "error" |>> { () =>
        throw new Error("an error") with NoStackTrace; Ok("Wont get here...")
      }
    }.toRoutes()
    val req = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/error")))

    assertEquals(
      service(req).value.unsafeRunSync().getOrElse(Response.notFound).status,
      Status.InternalServerError
    )
  }

  test("A RhoRoutes execution should give a None for missing route") {
    val service = new RhoRoutes[IO] {}.toRoutes()
    val req = Request[IO](Method.GET, Uri(path = Path.unsafeFromString("/missing")))
    assertEquals(
      service(req).value.unsafeRunSync().getOrElse(Response.notFound).status,
      Status.NotFound
    )
  }

  test("A RhoRoutes and method should concatenate service") {
    val routes1 = new RhoRoutes[IO] {
      GET / "foo1" |>> "Foo1"
    }
    val routes2 = new RhoRoutes[IO] {
      GET / "foo2" |>> "Foo2"
    }
    val both: RhoRoutes[IO] = routes1 and routes2
    val bothRoutes = both.toRoutes()

    assertEquals(both.getRoutes, routes1.getRoutes ++ routes2.getRoutes)

    val req1 = Request[IO](uri = uri"foo1")
    assertEquals(
      getBody(bothRoutes(req1).value.unsafeRunSync().getOrElse(Response.notFound).body),
      "Foo1"
    )

    val req2 = Request[IO](uri = uri"foo2")
    assertEquals(
      getBody(bothRoutes(req2).value.unsafeRunSync().getOrElse(Response.notFound).body),
      "Foo2"
    )
  }

  test("A RhoRoutes prefix operator should prefix a RhoRoutes") {
    val routes1 = new RhoRoutes[IO] {
      GET / "bar" |>> "bar"
    }

    val routes2: HttpRoutes[IO] = ("foo" /: routes1).toRoutes()

    val req1 = Request[IO](uri = Uri(path = Path.unsafeFromString("/foo/bar")))
    assertEquals(
      getBody(routes2(req1).value.unsafeRunSync().getOrElse(Response.notFound).body),
      "bar"
    )
  }
}
