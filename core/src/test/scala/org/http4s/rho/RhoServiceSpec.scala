package org.http4s
package rho

import org.specs2.mutable.Specification
import java.util.concurrent.atomic.AtomicInteger
import scalaz.concurrent.Task

class RhoServiceSpec extends Specification with RequestRunner {

  def Get(s: String, h: Header*): Request = Request(Method.GET, Uri.fromString(s).getOrElse(sys.error("Failed.")), headers = Headers(h: _*))

  val service = new RhoService {
    GET +? param("foo", "bar") |>> { foo: String => Ok(s"just root with parameter 'foo=$foo'") }

    GET / "" +? param("foo", "bar") |>> { foo: String => Ok("this definition should be hidden by the previous definition") }

    GET / "hello" |>> Ok("route1")

    GET / 'hello |>> { hello: String => Ok("route2") }

    GET / "hello" / "world" |>> Ok("route3")

    // keep the function for 'reverse routing'
    val reverseQuery = GET / "hello" / "headers" +? param[Int]("foo") |>> { foo: Int => Ok("route" + foo) }

    GET / "hello" / "reverse" |>> { () => reverseQuery(0) }

    GET / "hello" / "default" / "parameter" +? param[Int]("some", 23) |>> { s: Int => Ok("some:" + s) }

    // Routes that will have different headers/query string requirements should work together
    GET / "hello" / "compete" +? param[Int]("foo") |>> { foo: Int => Ok("route" + foo) }

    GET / "hello" / "compete" +? param[String]("foo") |>> { foo: String => Ok("route6_" + foo) }

    GET / "hello" / "compete" |>> Ok("route7")

    // Testing query params
    GET / "query" / "twoparams" +? param[Int]("foo") & param[String]("bar") |>> { (foo: Int, bar: String) =>
      Ok("twoparams" + foo + bar)
    }

    GET / "query" / "twoparams2" +? param[Int]("foo") & param[Option[String]]("bar") |>> { (foo: Int, bar: Option[String]) =>
      Ok("twoparams2_" + foo + bar.getOrElse("cat"))
    }

    GET / "variadic" / * |>> { tail: Seq[String] => Ok("route8_" + tail.mkString("/")) }

    val or = "or1" || "or2"
    GET / or |>> { () => Ok("route9") }

    GET / "orders" / pathVar[Int]("id") |>> { id: Int => Ok(id.toString) }

    GET / "options" +? param[Option[String]]("foo") |>> { os: Option[String] => Ok(os.getOrElse("None")) }

    GET / "seq" +? param[Seq[String]]("foo") |>> { os: Seq[String] => Ok(os.mkString(" ")) }

    GET / "seq" +? param[Seq[Int]]("foo") |>> { os: Seq[Int] => Ok(os.mkString(" ")) }

    GET / "withreq" +? param[String]("foo") |>> { (req: Request, foo: String) => Ok(s"req $foo") }

    val rootSome = root / "some"
    GET / rootSome |>> Ok("root to some")

    GET / "directTask" |>> {
      val i = new AtomicInteger(0)
      Task(s"${i.getAndIncrement}")
    }

    GET / "terminal" / "" |>> "terminal/"

    GET / "terminal" |>> "terminal"
  }

  "RhoService" should {

    "Handle definition without a path, which points to '/'" in {
      val request = Request(Method.GET, Uri(path = "/"))
      checkOk(request) should_== "just root with parameter 'foo=bar'"
    }

    "Handle definition without a path but with a parameter" in {
      val request = Request(Method.GET, Uri.fromString("/?foo=biz").getOrElse(sys.error("Fail.")))
      checkOk(request) should_== "just root with parameter 'foo=biz'"
    }

    "Consider PathMatch(\"\") a NOOP" in {
      val service = new RhoService {
        GET / "" / "foo" |>> Ok("bar")
      }.toService

      val req1 = Request(Method.GET, Uri(path = "/foo"))
      getBody(service(req1).run.get.body) should_== "bar"

      val req2 = Request(Method.GET, Uri(path = "//foo"))
      getBody(service(req2).run.get.body) should_== "bar"
    }

    "Execute a route with no params" in {
      val req = Get("/hello")
      checkOk(req) should_== "route1"
    }

    "Execute a route with a single param" in {
      val req = Get("/world")
      checkOk(req) should_== "route2"
    }

    "Execute a route with a single param" in {
      val req = Get("/hello/world")
      checkOk(req) should_== "route3"
    }

    "Execute a route with a query" in {
      val req = Get("/hello/headers?foo=4")
      checkOk(req) should_== "route4"
    }

    "Provide 'reverse routing' characteristics" in {
      val req = Get("/hello/reverse")
      checkOk(req) should_== "route0"
    }

    "Fail a route with a missing query" in {
      val req = Get("/hello/headers")
      checkError(req) should_== "Missing query param: foo"
    }

    "Fail a route with an invalid query" in {
      val req = Get("/hello/headers?foo=bar")
      checkError(req) should_== "Invalid Number Format: \"bar\""
    }

    "Execute a route with multiple query with parameters" in {
      "query" / "twoparams"
      val req = Get("/query/twoparams?foo=5&bar=cat")
      checkOk(req) should_== "twoparams5cat"
    }

    "Execute a route with multiple query with parameters, one optional" in {
      "query" / "twoparams"
      val req = Get("/query/twoparams2?foo=5")
      checkOk(req) should_== "twoparams2_5cat"
    }

    "Execute a route with a query to override parameter default" in {
      val req = Get("/hello/default/parameter?some=42")
      checkOk(req) should_== "some:42"
    }

    "Execute a route with a query with default parameter" in {
      val req = Get("/hello/default/parameter")
      checkOk(req) should_== "some:23"
    }

    "Fail a route with an invalid parameter type" in {
      val req = Get("/hello/default/parameter?some=a")
      checkError(req) should_== "Invalid Number Format: \"a\""
    }

    "Execute a route with a competing query" in {
      val req1 = Get("/hello/compete?foo=5")
      val req2 = Get("/hello/compete?foo=bar")
      val req3 = Get("/hello/compete")
      (checkOk(req1) should_== "route5") and
        (checkOk(req2) should_== "route6_bar") and
        (checkOk(req3) should_== "route7")
    }

    "Execute a variadic route" in {
      val req1 = Get("/variadic")
      val req2 = Get("/variadic/one")
      val req3 = Get("/variadic/one/two")

      (checkOk(req1) should_== "route8_") and
        (checkOk(req2) should_== "route8_one") and
        (checkOk(req3) should_== "route8_one/two")
    }

    "Perform path 'or' logic" in {
      val req1 = Get("/or1")
      val req2 = Get("/or2")
      (checkOk(req1) should_== "route9") and
        (checkOk(req2) should_== "route9")
    }

    "Work with options" in {
      val req1 = Get("/options")
      val req2 = Get("/options?foo=bar")
      (checkOk(req1) should_== "None") and
        (checkOk(req2) should_== "bar")
    }

    "Work with collections" in {
      val req1 = Get("/seq")
      val req2 = Get("/seq?foo=bar")
      val req3 = Get("/seq?foo=1&foo=2")
      (checkOk(req1) should_== "") and
        (checkOk(req2) should_== "bar") and
        (checkOk(req3) should_== "1 2")
    }

    "Provide the request if desired" in {
      val req = Get("/withreq?foo=bar")
      checkOk(req) should_== "req bar"
    }

    "Level one path definition to /some" in {
      val req1 = Request(Method.GET, Uri(path = "/some"))
      checkOk(req1) should_== "root to some"
    }

    "Execute a directly provided Task every invocation" in {
      val req = Request(Method.GET, Uri(path = "directTask"))
      checkOk(req) should_== "0"
      checkOk(req) should_== "1"
    }

    "Interpret uris ending in '/' differently than those without" in {
      val req1 = Request(Method.GET, Uri(path = "terminal/"))
      checkOk(req1) should_== "terminal/"

      val req2 = Request(Method.GET, Uri(path = "terminal"))
      checkOk(req2) should_== "terminal"
    }

    ///// Order of execution tests /////////////////////
    "Attempt to evaluate params in order" in {
      val service = new RhoService {
        GET / "foo" +? param[Int]("bar") |>> { i: Int => Ok(s"Int: $i") }
        GET / "foo" +? param[String]("bar") |>> { i: String => Ok(s"String: $i") }
        GET / "foo" |>> Ok("none")
      }.toService

      val req1 = Request(Method.GET, Uri(path = "/foo").+?("bar", "0"))
      getBody(service(req1).run.get.body) must_== "Int: 0"

      val req2 = Request(Method.GET, Uri(path = "/foo").+?("bar", "s"))
      getBody(service(req2).run.get.body) must_== "String: s"

      val req3 = Request(Method.GET, Uri(path = "/foo"))
      getBody(service(req3).run.get.body) must_== "none"
    }

    "Fail to match more specific routes defined after a less specific route" in {
      val service = new RhoService {
        GET / "foo" +? param[String]("bar") |>> { i: String => Ok(s"String: $i") }
        GET / "foo" +? param[Int]("bar") |>> { i: Int => Ok(s"Int: $i") }
      }.toService

      val req1 = Request(Method.GET, Uri(path = "/foo").+?("bar", "0"))
      getBody(service(req1).run.get.body) must_== "String: 0"

      val req2 = Request(Method.GET, Uri(path = "/foo").+?("bar", "s"))
      getBody(service(req2).run.get.body) must_== "String: s"
    }

    "Match an empty Option over a bare route" in {
      val service = new RhoService {
        GET / "foo" +? param[Option[String]]("bar") |>> { o: Option[String] =>
          o.map(s => s"String: $s").getOrElse("none")
        }

        GET / "foo" |>> Ok(s"failure")
      }.toService

      val req1 = Request(Method.GET, Uri(path = "/foo").+?("bar", "s"))
      getBody(service(req1).run.get.body) must_== "String: s"

      val req2 = Request(Method.GET, Uri(path = "/foo"))
      getBody(service(req2).run.get.body) must_== "none"
    }




  ////////////////////////////////////////////////////
    "Handle errors in the route actions" in {
      val service = new RhoService {
        GET / "error" |>> { () => throw new Error("an error"); Ok("Wont get here...") }
      }.toService
      val req = Request(Method.GET, Uri(path = "/error"))
      service(req).run.get.status must equalTo(Status.InternalServerError)
    }

    "give a None for missing route" in {
      val service = new RhoService {}.toService
      val req = Request(Method.GET, Uri(path = "/missing"))
      service(req).run must_== None
    }
  }

}
