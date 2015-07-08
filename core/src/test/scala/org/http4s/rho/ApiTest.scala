package org.http4s
package rho

import bits.MethodAliases._
import bits.ResponseGeneratorInstances._

import bits.HeaderAST.{TypedHeader, HeaderAnd}
import bits.{PathTree, ParserSuccess, ValidationFailure}

import org.specs2.mutable._
import shapeless.HNil
import scalaz.{-\/, \/-}
import scalaz.concurrent.Task
import scalaz.stream.Process
import scodec.bits.ByteVector

// TODO: these tests are a bit of a mess
class ApiTest extends Specification {
  val lenheader = headers.`Content-Length`(4)
  val etag = headers.ETag("foo")

  val RequireETag = exists(headers.ETag)
  val RequireNonZeroLen = existsAnd(headers.`Content-Length`){ h => h.length != 0 }

  def fetchETag(p: Task[Option[Response]]): String = {
    val resp = p.run

    val mvalue = for {
      r <- resp
      h <- r.headers.get(headers.ETag)
    } yield h.value

    mvalue.getOrElse(sys.error("No ETag: " + resp))
  }

  "RhoDsl bits" should {
    "Combine validators" in {
      RequireETag && RequireNonZeroLen should_== TypedHeader(HeaderAnd(RequireETag.rule, RequireNonZeroLen.rule))
    }

    "Fail on a bad request" in {
      val badreq = Request().withHeaders(Headers(lenheader))
      val res = PathTree.ValidationTools.ensureValidHeaders((RequireETag && RequireNonZeroLen).rule,badreq)

      res must beAnInstanceOf[ValidationFailure]
      res.asInstanceOf[ValidationFailure].response.run.resp.status must_== Status.BadRequest
    }

    "Match captureless route" in {
      val c = RequireETag && RequireNonZeroLen

      val req = Request().withHeaders(Headers(etag, lenheader))
      PathTree.ValidationTools.ensureValidHeaders(c.rule, req) should_== ParserSuccess(HNil)
    }

    "Capture params" in {
      val req = Request().withHeaders(Headers(etag, lenheader))
      Seq({
        val c2 = capture(headers.`Content-Length`) && RequireETag
        PathTree.ValidationTools.ensureValidHeaders(c2.rule, req) should_== ParserSuccess(lenheader::HNil)
      }, {
        val c3 = capture(headers.`Content-Length`) && capture(headers.ETag)
        PathTree.ValidationTools.ensureValidHeaders(c3.rule, req) should_== ParserSuccess(etag::lenheader::HNil)
      }).reduce( _ and _)
    }

    "Map header params" in {
      val req = Request().withHeaders(Headers(etag, lenheader))
      val c = captureMap(headers.`Content-Length`)(_.length)
      PathTree.ValidationTools.ensureValidHeaders(c.rule, req) should_== ParserSuccess(4::HNil)
    }

    "Map with possible default" in {
      val req = Request().withHeaders(Headers(etag, lenheader))

      val c1 = captureMapR(headers.`Content-Length`)(r => \/-(r.length))
      PathTree.ValidationTools.ensureValidHeaders(c1.rule, req) should_== ParserSuccess(4::HNil)

      val r2 = Ok("Foo")
      val c2 = captureMapR(headers.`Content-Length`)(_ => -\/(r2))
      PathTree.ValidationTools.ensureValidHeaders(c2.rule, req) should_== ValidationFailure(r2)

      val c3 = captureMapR(headers.`Access-Control-Allow-Credentials`, Some(r2))(_ => ???)
      PathTree.ValidationTools.ensureValidHeaders(c3.rule, req) should_== ValidationFailure(r2)
    }

    "Append headers to a Route" in {

      val path = POST / "hello" / 'world +? param[Int]("fav")
      val validations = existsAnd(headers.`Content-Length`){ h => h.length != 0 }


      val route = (path >>> validations >>> capture(headers.ETag)).decoding(EntityDecoder.text) runWith
        {(world: String, fav: Int, tag: headers.ETag, body: String) =>

          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .putHeaders(headers.ETag("foo"))
        }

      val body = Process.emit(ByteVector("cool".getBytes))
      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders(headers.ETag("foo"))
        .withBody("cool")
        .run

      val resp = route(req).run.get
      resp.headers.get(headers.ETag).get.value should_== "foo"

    }

    "Run || routes" in {
      val p1 = "one" / 'two
      val p2 = "three" / 'four

      val f = GET / (p1 || p2) runWith { (s: String) => Ok("").withHeaders(headers.ETag(s)) }

      val req1 = Request(uri = Uri.fromString("/one/two").getOrElse(sys.error("Failed.")))
      fetchETag(f(req1)) should_== "two"

      val req2 = Request(uri = Uri.fromString("/three/four").getOrElse(sys.error("Failed.")))
      fetchETag(f(req2)) should_== "four"
    }

    "Execute a complicated route" in {

      val path = POST / "hello" / 'world +? param[Int]("fav")
      val validations = existsAnd(headers.`Content-Length`){ h => h.length != 0 } &&
        capture(headers.ETag)

      val route =
        (path >>> validations).decoding(EntityDecoder.text) runWith {(world: String, fav: Int, tag: headers.ETag, body: String) =>

          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .putHeaders(headers.ETag("foo"))
        }

      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders( headers.ETag("foo"))
        .withBody("cool")
        .run

      val resp = route(req).run.get
      resp.headers.get(headers.ETag).get.value should_== "foo"
    }

    "Deal with 'no entity' responses" in {
      val route = GET / "foo" runWith { () => SwitchingProtocols() }
      val req = Request(GET, uri = Uri.fromString("/foo").getOrElse(sys.error("Fail")))

      val result = route(req).run.get
      result.headers.size must_== 0
      result.status must_== Status.SwitchingProtocols
    }
  }

  "RequestLineBuilder" should {
    "be made from TypedPath and TypedQuery" in {
      val path = pathMatch("foo")
      val q = param[Int]("bar")
      path +? q should_== RequestLineBuilder(path.rule, q.rule)
    }

    "append to a TypedPath" in {
      val requestLine = pathMatch("foo") +? param[Int]("bar")
      (pathMatch("hello") / requestLine).isInstanceOf[RequestLineBuilder[_]] should_== true
    }
  }

  "PathValidator" should {

    def check(p: Task[Option[Response]], s: String) = {
      p.run.get.headers.get(headers.ETag).get.value should_== s
    }

    "traverse a captureless path" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { () => Ok("Cool.").withHeaders(headers.ETag("foo")) }
      check(f(req), "foo")
    }

    "Not match a path to long" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello/world").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { () => Ok("Cool.").withHeaders(headers.ETag("foo")) }
      val r = f(req).run
      r should_== None
    }

    "capture a variable" in {
      val stuff = GET / 'hello
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { str: String => Ok("Cool.").withHeaders(headers.ETag(str)) }
      check(f(req), "hello")
    }

    "work directly" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { () => Ok("Cool.").withHeaders(headers.ETag("foo")) }

      check(f(req), "foo")
    }

    "capture end with nothing" in {
      val stuff = GET / "hello" / *
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))
      val f = stuff runWith { path: List[String] => Ok("Cool.").withHeaders(headers.ETag(if (path.isEmpty) "go" else "nogo")) }

      check(f(req), "go")
    }

    "capture remaining" in {
      val stuff = GET / "hello" / *
      val req = Request(uri = Uri.fromString("/hello/world/foo").getOrElse(sys.error("Failed.")))
      val f = stuff runWith { path: List[String] => Ok("Cool.").withHeaders(headers.ETag(path.mkString)) }

      check(f(req), "worldfoo")
    }
  }

  "Query validators" should {
    "get a query string" in {
      val path = GET / "hello" +? param[Int]("jimbo")
      val req = Request(uri = Uri.fromString("/hello?jimbo=32").getOrElse(sys.error("Failed.")))

      val route = path runWith { i: Int => Ok("stuff").withHeaders(headers.ETag((i + 1).toString)) }

      fetchETag(route(req)) should_== "33"

    }
  }

  "Decoders" should {
    "Decode a body" in {
      val reqHeader = existsAnd(headers.`Content-Length`){ h => h.length < 10 }

      val path = POST / "hello" >>> reqHeader


      val req1 = Request(POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
                    .withBody("foo")
                    .run

      val req2 = Request(POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
        .withBody("0123456789") // length 10
        .run

      val route = path.decoding(EntityDecoder.text) runWith { str: String =>
        Ok("stuff").withHeaders(headers.ETag(str))
      }

      fetchETag(route(req1)) should_== "foo"
      route(req2).run.get.status should_== Status.BadRequest
    }

    "Allow the infix operator syntax" in {
      val path = POST / "hello"

      val req = Request(POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
        .withBody("foo")
        .run

      val route = path ^ EntityDecoder.text runWith { str: String =>
        Ok("stuff").withHeaders(headers.ETag(str))
      }

      fetchETag(route(req)) should_== "foo"
    }

    "Fail on a header" in {
      val path = GET / "hello"

      val req = Request(uri = uri("/hello"))
                  .withHeaders(Headers(headers.`Content-Length`("foo".length)))

      val reqHeader = existsAnd(headers.`Content-Length`){ h => h.length < 2}
      val route1 = path.validate(reqHeader) runWith { () =>
        Ok("shouldn't get here.")
      }

      route1(req).run.get.status should_== Status.BadRequest

      val reqHeaderR = existsAndR(headers.`Content-Length`){ h => Some(Unauthorized("Foo."))}
      val route2 = path.validate(reqHeaderR) runWith { () =>
        Ok("shouldn't get here.")
      }

      route2(req).run.get.status should_== Status.Unauthorized
    }

    "Fail on a query" in {
      val path = GET / "hello"

      val req = Request(uri = uri("/hello?foo=bar"))
        .withHeaders(Headers(headers.`Content-Length`("foo".length)))

      val route1 = (path +? param[Int]("foo")).runWith { i: Int =>
        Ok("shouldn't get here.")
      }

      route1(req).run.get.status should_== Status.BadRequest

      val route2 = (path +? paramR[String]("foo", (_: String) => Some(Unauthorized("foo")))).runWith { str: String =>
        Ok("shouldn't get here.")
      }

      route2(req).run.get.status should_== Status.Unauthorized
    }
  }
}
