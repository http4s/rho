package org.http4s
package rho

import bits.MethodAliases._
import bits.ResponseGeneratorInstances._

import bits.HeaderAST.{TypedHeader, HeaderAnd}
import bits.{PathTree, SuccessResponse, FailureResponse}
import org.http4s.rho.bits.QueryAST.QueryCapture

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

  val RequireThrowException = existsAnd(headers.`Content-Length`){ h => throw new RuntimeException("this could happen") }

  def fetchETag(p: Task[Response]): String = {
    val resp = p.run
    val mvalue = resp.headers.get(headers.ETag).map(_.value)

    mvalue.getOrElse(sys.error("No ETag: " + resp))
  }

  "RhoDsl bits" should {
    "Combine validators" in {
      RequireETag && RequireNonZeroLen should_== TypedHeader(HeaderAnd(RequireETag.rule, RequireNonZeroLen.rule))
    }

    "Fail on a bad request" in {
      val badreq = Request().withHeaders(Headers(lenheader))
      val res = PathTree.ValidationTools.ensureValidHeaders((RequireETag && RequireNonZeroLen).rule,badreq)

      res must beAnInstanceOf[FailureResponse]
      res.asInstanceOf[FailureResponse].toResponse.run.status must_== Status.BadRequest
    }
                                                                                        
    "Fail on a bad request 2" in {
      val req = Request().withHeaders(Headers(lenheader))
      val res = PathTree.ValidationTools.ensureValidHeaders(RequireThrowException.rule, req)                                                            
      res must beAnInstanceOf[FailureResponse]                                                                                                          
      res.asInstanceOf[FailureResponse].toResponse.run.status must_== Status.BadRequest
      res.asInstanceOf[FailureResponse].toResponse.run.as[String].run must_== "this could happen"
    }

    "Match captureless route" in {
      val c = RequireETag && RequireNonZeroLen

      val req = Request().withHeaders(Headers(etag, lenheader))
      PathTree.ValidationTools.ensureValidHeaders(c.rule, req) should_== SuccessResponse(HNil)
    }

    "Capture params" in {
      val req = Request().withHeaders(Headers(etag, lenheader))
      Seq({
        val c2 = capture(headers.`Content-Length`) && RequireETag
        PathTree.ValidationTools.ensureValidHeaders(c2.rule, req) should_== SuccessResponse(lenheader::HNil)
      }, {
        val c3 = capture(headers.`Content-Length`) && capture(headers.ETag)
        PathTree.ValidationTools.ensureValidHeaders(c3.rule, req) should_== SuccessResponse(etag::lenheader::HNil)
      }).reduce( _ and _)
    }

    "Map header params" in {
      val req = Request().withHeaders(Headers(etag, lenheader))
      val c = captureMap(headers.`Content-Length`)(_.length)
      PathTree.ValidationTools.ensureValidHeaders(c.rule, req) should_== SuccessResponse(4::HNil)
    }

    "Map header params with exception" in {
      val req = Request().withHeaders(Headers(etag, lenheader))
      val c = captureMap(headers.`Content-Length`)(_.length / 0)
      PathTree.ValidationTools.ensureValidHeaders(c.rule, req) must beAnInstanceOf[FailureResponse]
    }

    "Map with possible default" in {
      val req = Request().withHeaders(Headers(etag, lenheader))

      val c1 = captureMapR(headers.`Content-Length`)(r => \/-(r.length))
      PathTree.ValidationTools.ensureValidHeaders(c1.rule, req) should_== SuccessResponse(4::HNil)

      val r2 = Gone("Foo")
      val c2 = captureMapR(headers.`Content-Length`)(_ => -\/(r2))
      val v1 = PathTree.ValidationTools.ensureValidHeaders(c2.rule, req)
      v1 must beAnInstanceOf[FailureResponse]
      v1.asInstanceOf[FailureResponse].toResponse.run.status must_== r2.run.resp.status

      val c3 = captureMapR(headers.`Access-Control-Allow-Credentials`, Some(r2))(_ => ???)
      val v2 = PathTree.ValidationTools.ensureValidHeaders(c3.rule, req)
      v2 must beAnInstanceOf[FailureResponse]
      v2.asInstanceOf[FailureResponse].toResponse.run.status must_== r2.run.resp.status
    }

    "Append headers to a Route" in {

      val path = POST / "hello" / 'world +? param[Int]("fav")
      val validations = existsAnd(headers.`Content-Length`){ h => h.length != 0 }


      val route = (path >>> validations >>> capture(headers.ETag)).decoding(EntityDecoder.text) runWith {
        (world: String, fav: Int, tag: headers.ETag, body: String) =>
          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .putHeaders(headers.ETag("foo"))
        }

      val body = Process.emit(ByteVector("cool".getBytes))
      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders(headers.ETag("foo"))
        .withBody("cool")
        .run

      val resp = route(req).run
      resp.headers.get(headers.ETag).get.value should_== "foo"

    }

    "accept compound or sequential header rules" in {

      val path = POST / "hello" / 'world
      val lplus1 = captureMap(headers.`Content-Length`)(_.length + 1)


      val route1 = (path >>> lplus1 >>> capture(headers.ETag)).decoding(EntityDecoder.text) runWith {
        (world: String, lplus1: Int, tag: headers.ETag, body: String) =>
          Ok("")
      }

      val route2 = (path >>> (lplus1 && capture(headers.ETag))).decoding(EntityDecoder.text) runWith {
        (world: String, lplus1: Int, tag: headers.ETag, body: String) =>
          Ok("")
      }

      val body = Process.emit(ByteVector("cool".getBytes))
      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders(headers.ETag("foo"))
        .withBody("cool")
        .run

      route1(req).run.status should_== Status.Ok
      route2(req).run.status should_== Status.Ok

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

      val resp = route(req).run
      resp.headers.get(headers.ETag).get.value should_== "foo"
    }

    "Deal with 'no entity' responses" in {
      val route = GET / "foo" runWith { () => SwitchingProtocols() }
      val req = Request(GET, uri = Uri.fromString("/foo").getOrElse(sys.error("Fail")))

      val result = route(req).run
      result.headers.size must_== 0
      result.status must_== Status.SwitchingProtocols
    }
  }

  "RequestLineBuilder" should {
    "be made from TypedPath and TypedQuery" in {
      val path = pathMatch("foo")
      val q = param[Int]("bar")
      path +? q should_== RequestLineBuilder(path.rule, QueryCapture(q))
    }

    "append to a TypedPath" in {
      val requestLine = pathMatch("foo") +? param[Int]("bar")
      (pathMatch("hello") / requestLine).isInstanceOf[RequestLineBuilder[_]] should_== true
    }
  }

  "PathValidator" should {

    def check(p: Task[Response], s: String) = {
      p.run.headers.get(headers.ETag).get.value should_== s
    }

    "traverse a captureless path" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = stuff runWith { () => Ok("Cool.").withHeaders(headers.ETag("foo")) }
      check(f(req), "foo")
    }

    "Not match a path to long" in {
      val stuff = GET / "hello"
      val req = Request(uri = uri("/hello/world"))

      val f = stuff runWith { () => Ok("Shouldn't get here.") }
      val r = f(req).run
      r.status should_== Status.NotFound
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

    "accept compound or sequential query rules" in {
      val path = GET / "hello"

      val req = Request(uri = uri("/hello?foo=bar&baz=1"))

      val route1 = (path +? param[String]("foo") & param[Int]("baz")) runWith { (_: String, _: Int) =>
        Ok("")
      }

      route1(req).run.status should_== Status.Ok

      val route2 = (path +? (param[String]("foo") and param[Int]("baz"))) runWith { (_: String, _: Int) =>
        Ok("")
      }

      route2(req).run.status should_== Status.Ok
    }

    "Work as applicatives" in {
      import scalaz._
      import Scalaz._

      val path = GET / "hello"
      val req = Request(uri = uri("/hello?foo=bar&baz=1"))

      case class FooBar(foo: String, baz: Int)

      val p = (param[String]("foo") |@| param[Int]("baz"))(FooBar.apply)

      val route1 = (path +? p) runWith { _: FooBar => Ok("") }

      route1(req).run.status should_== Status.Ok
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
      route(req2).run.status should_== Status.BadRequest
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

      route1(req).run.status should_== Status.BadRequest

      val reqHeaderR = existsAndR(headers.`Content-Length`){ h => Some(Unauthorized("Foo."))}
      val route2 = path.validate(reqHeaderR) runWith { () =>
        Ok("shouldn't get here.")
      }

      route2(req).run.status should_== Status.Unauthorized
    }

    "Fail on a query" in {
      val path = GET / "hello"

      val req = Request(uri = uri("/hello?foo=bar"))
        .replaceAllHeaders(Headers(headers.`Content-Length`("foo".length)))

      val route1 = (path +? param[Int]("foo")) runWith { i: Int =>
        Ok("shouldn't get here.")
      }

      route1(req).run.status should_== Status.BadRequest

      val route2 = (path +? paramR[String]("foo"){ _: String => Some(Unauthorized("foo")) })  runWith { str: String =>
        Ok("shouldn't get here.")
      }

      route2(req).run.status should_== Status.Unauthorized
    }
  }

  "Path prepending" should {

    val req = Request(uri=uri("/foo/bar"))
    val respMsg = "Result"

    "Work for a PathBuilder" in {
      val tail = GET / "bar"
      val all = "foo" /: tail
      all.runWith(respMsg).apply(req).run.as[String].run === respMsg
    }

    "Work for a QueryBuilder" in {
      val tail = GET / "bar" +? param[String]("str")
      val all = "foo" /: tail
      all.runWith{ q: String => respMsg + q}.apply(req.copy(uri=uri("/foo/bar?str=answer"))).run.as[String].run === respMsg + "answer"
    }

    "Work for a Router" in {
      val tail = GET / "bar" >>> RequireETag
      val all = "foo" /: tail
      all.runWith(respMsg).apply(req.copy(uri=uri("/foo/bar")).putHeaders(etag)).run.as[String].run === respMsg
    }
  }
}
