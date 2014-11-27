package org.http4s
package rho

import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.bits.ResponseGeneratorInstances._

import org.http4s.rho.bits.HeaderAST.{TypedHeader, HeaderAnd}
import org.http4s.rho.bits.{RhoPathTree, ParserSuccess, ValidationFailure}

import org.specs2.mutable._
import shapeless.HNil
import scalaz.concurrent.Task
import scalaz.stream.Process
import scodec.bits.ByteVector

// TODO: these tests are a bit of a mess
class ApiTest extends Specification {
  val lenheader = Header.`Content-Length`(4)
  val etag = Header.ETag("foo")

  val RequireETag = require(Header.ETag)
  val RequireNonZeroLen = requireThat(Header.`Content-Length`){ h => h.length != 0 }

  def fetchETag(p: Task[Option[Response]]): String = {
    val resp = p.run

    val mvalue = for {
      r <- resp
      h <- r.headers.get(Header.ETag)
    } yield h.value

    mvalue.getOrElse(sys.error("No ETag: " + resp))
  }

  "RhoDsl bits" should {
    "Combine validators" in {
      RequireETag && RequireNonZeroLen should_== TypedHeader(HeaderAnd(RequireETag.rule, RequireNonZeroLen.rule))
    }

    "Fail on a bad request" in {
      val badreq = Request().withHeaders(Headers(lenheader))
      RhoPathTree.ValidationTools.ensureValidHeaders((RequireETag && RequireNonZeroLen).rule,badreq) should_==
                ValidationFailure(s"Missing header: ${etag.name}")
    }

    "Match captureless route" in {
      val c = RequireETag && RequireNonZeroLen

      val req = Request().withHeaders(Headers(etag, lenheader))
      RhoPathTree.ValidationTools.ensureValidHeaders(c.rule, req) should_== ParserSuccess(HNil)
    }

    "Capture params" in {
      val req = Request().withHeaders(Headers(etag, lenheader))
      Seq({
        val c2 = capture(Header.`Content-Length`) && RequireETag
        RhoPathTree.ValidationTools.ensureValidHeaders(c2.rule, req) should_== ParserSuccess(lenheader::HNil)
      }, {
        val c3 = capture(Header.`Content-Length`) && capture(Header.ETag)
        RhoPathTree.ValidationTools.ensureValidHeaders(c3.rule, req) should_== ParserSuccess(etag::lenheader::HNil)
      }).reduce( _ and _)
    }

    "Map header params" in {
      val req = Request().withHeaders(Headers(etag, lenheader))
      val c = requireMap(Header.`Content-Length`)(_.length)
      RhoPathTree.ValidationTools.ensureValidHeaders(c.rule, req) should_== ParserSuccess(4::HNil)
    }

    "Append headers to a Route" in {

      val path = POST / "hello" / 'world +? param[Int]("fav")
      val validations = requireThat(Header.`Content-Length`){ h => h.length != 0 }


      val route = (path >>> validations >>> capture(Header.ETag)).decoding(EntityDecoder.text) runWith
        {(world: String, fav: Int, tag: Header.ETag, body: String) =>

          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .putHeaders(Header.ETag("foo"))
        }

      val body = Process.emit(ByteVector("cool".getBytes))
      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders(Header.ETag("foo"))
        .withBody("cool")
        .run

      val resp = route(req).run.get
      resp.headers.get(Header.ETag).get.value should_== "foo"

    }

    "Run || routes" in {
      val p1 = "one" / 'two
      val p2 = "three" / 'four

      val f = GET / (p1 || p2) runWith { (s: String) => Ok("").withHeaders(Header.ETag(s)) }

      val req1 = Request(uri = Uri.fromString("/one/two").getOrElse(sys.error("Failed.")))
      fetchETag(f(req1)) should_== "two"

      val req2 = Request(uri = Uri.fromString("/three/four").getOrElse(sys.error("Failed.")))
      fetchETag(f(req2)) should_== "four"
    }

    "Execute a complicated route" in {

      val path = POST / "hello" / 'world +? param[Int]("fav")
      val validations = requireThat(Header.`Content-Length`){ h => h.length != 0 } &&
        capture(Header.ETag)

      val route =
        (path >>> validations).decoding(EntityDecoder.text) runWith {(world: String, fav: Int, tag: Header.ETag, body: String) =>

          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .putHeaders(Header.ETag("foo"))
        }

      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders( Header.ETag("foo"))
        .withBody("cool")
        .run

      val resp = route(req).run.get
      resp.headers.get(Header.ETag).get.value should_== "foo"
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
      p.run.get.headers.get(Header.ETag).get.value should_== s
    }

    "traverse a captureless path" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { () => Ok("Cool.").withHeaders(Header.ETag("foo")) }
      check(f(req), "foo")
    }

    "Not match a path to long" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello/world").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { () => Ok("Cool.").withHeaders(Header.ETag("foo")) }
      val r = f(req).run
      r should_== None
    }

    "capture a variable" in {
      val stuff = GET / 'hello
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { str: String => Ok("Cool.").withHeaders(Header.ETag(str)) }
      check(f(req), "hello")
    }

    "work directly" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { () => Ok("Cool.").withHeaders(Header.ETag("foo")) }

      check(f(req), "foo")
    }

    "capture end with nothing" in {
      val stuff = GET / "hello" / *
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))
      val f = stuff runWith { path: List[String] => Ok("Cool.").withHeaders(Header.ETag(if (path.isEmpty) "go" else "nogo")) }

      check(f(req), "go")
    }

    "capture remaining" in {
      val stuff = GET / "hello" / *
      val req = Request(uri = Uri.fromString("/hello/world/foo").getOrElse(sys.error("Failed.")))
      val f = stuff runWith { path: List[String] => Ok("Cool.").withHeaders(Header.ETag(path.mkString)) }

      check(f(req), "worldfoo")
    }
  }

  "Query validators" should {
    "get a query string" in {
      val path = GET / "hello" +? param[Int]("jimbo")
      val req = Request(uri = Uri.fromString("/hello?jimbo=32").getOrElse(sys.error("Failed.")))

      val route = path runWith { i: Int => Ok("stuff").withHeaders(Header.ETag((i + 1).toString)) }

      fetchETag(route(req)) should_== "33"

    }
  }

  "Decoders" should {
    "Decode a body" in {
      val reqHeader = requireThat(Header.`Content-Length`){ h => h.length < 10 }

      val path = POST / "hello" >>> reqHeader


      val req1 = Request(POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
                    .withBody("foo")
                    .run

      val req2 = Request(POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
        .withBody("0123456789") // length 10
        .run

      val route = path.decoding(EntityDecoder.text) runWith { str: String =>
        Ok("stuff").withHeaders(Header.ETag(str))
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
        Ok("stuff").withHeaders(Header.ETag(str))
      }

      fetchETag(route(req)) should_== "foo"
    }

    "Fail on a header" in {
      val path = GET / "hello"
      val reqHeader = requireThat(Header.`Content-Length`){ h => h.length < 2}
      val body = Process.emit(ByteVector.apply("foo".getBytes()))
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")), body = body)
        .withHeaders(Headers(Header.`Content-Length`("foo".length)))

      val route = path.validate(reqHeader).decoding(EntityDecoder.text) runWith { str: String =>
        Ok("stuff").withHeaders(Header.ETag(str))
      }

      val result = route(req)
      result.run.get.status should_== Status.BadRequest
    }
  }
}
