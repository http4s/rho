package org.http4s
package rho

import org.http4s.headers.{ETag, `Content-Length`}
import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.bits.RequestAST.AndRule
import org.http4s.rho.bits.ResponseGeneratorInstances._
import org.http4s.rho.bits._
import org.specs2.mutable._
import scodec.bits.ByteVector
import shapeless.{::, HList, HNil}
import fs2.{Stream, Task}

// TODO: these tests are a bit of a mess
class ApiTest extends Specification {

  def runWith[T <: HList, F](exec: RouteExecutable[T])(f: F)(implicit hltf: HListToFunc[T, F]): Request => Task[MaybeResponse] = {
    val srvc = new RhoService { exec |>> f }.toService()
    srvc.apply(_: Request)
  }

  val lenheader = headers.`Content-Length`(4)
  val etag = ETag(ETag.EntityTag("foo"))

  val RequireETag = exists(ETag)
  val RequireNonZeroLen = existsAnd(headers.`Content-Length`){ h => h.length != 0 }

  val RequireThrowException = existsAnd(headers.`Content-Length`){ h => throw new RuntimeException("this could happen") }

  def fetchETag(p: Task[Response]): ETag = {
    val resp = p.unsafeRun()
    resp.headers.get(ETag).getOrElse(sys.error("No ETag: " + resp))
  }

  def checkETag(p: Task[MaybeResponse], s: String) =
    fetchETag(p.map(_.orNotFound)) must_== ETag(ETag.EntityTag(s))

  "RhoDsl bits" should {
    "Combine validators" in {
      RequireETag && RequireNonZeroLen should_== TypedHeader(AndRule(RequireETag.rule, RequireNonZeroLen.rule))
    }

    "Fail on a bad request" in {
      val badreq = Request().putHeaders(lenheader)
      val res = RuleExecutor.runRequestRules((RequireETag && RequireNonZeroLen).rule, badreq)

      res must beAnInstanceOf[FailureResponse]
      res.asInstanceOf[FailureResponse].toResponse.unsafeRun.status must_== Status.BadRequest
    }

    "Fail on a bad request 2" in {
      val req = Request().putHeaders(lenheader)
      val res = RuleExecutor.runRequestRules(RequireThrowException.rule, req)
      res must beAnInstanceOf[FailureResponse]
      res.asInstanceOf[FailureResponse].toResponse.unsafeRun.status must_== Status.InternalServerError
    }

    "Match captureless route" in {
      val c = RequireETag && RequireNonZeroLen

      val req = Request().putHeaders(etag, lenheader)
      RuleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(HNil)
    }

    "Capture params" in {
      val req = Request().putHeaders(etag, lenheader)
      Seq({
        val c2 = capture(headers.`Content-Length`) && RequireETag
        RuleExecutor.runRequestRules(c2.rule, req) should_== SuccessResponse(lenheader :: HNil)
      }, {
        val c3 = capture(headers.`Content-Length`) && capture(ETag)
        RuleExecutor.runRequestRules(c3.rule, req) should_== SuccessResponse(etag :: lenheader :: HNil)
      }).reduce(_ and _)
    }

    "Map header params" in {
      val req = Request().putHeaders(etag, lenheader)
      val c = captureMap(headers.`Content-Length`)(_.length)
      RuleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(4 :: HNil)
    }

    "Map header params with exception" in {
      val req = Request().putHeaders(etag, lenheader)
      val c = captureMap(headers.`Content-Length`)(_.length / 0)
      RuleExecutor.runRequestRules(c.rule, req) must beAnInstanceOf[FailureResponse]
    }

    "map simple header params into a complex type" in {
      case class Foo(age: Long, s: java.time.Instant)
      val paramFoo = captureMap(headers.`Content-Length`)(_.length) && captureMap(headers.Date)(_.date) map Foo.apply _

      val now = java.time.Instant.now()
      val path = GET / "hello" +? paramFoo
      val req = Request(
        uri = Uri.fromString("/hello?i=32&f=3.2&s=Asdf").right.getOrElse(sys.error("Failed.")),
        headers = Headers(headers.`Content-Length`(10), headers.Date(now))
      )

      val expectedFoo = Foo(10, now)
      val route = runWith(path) { (f: Foo) => Ok(s"stuff $f") }

      val result = route(req).unsafeRun.orNotFound
      result.status should_== Status.Ok
      RequestRunner.getBody(result.body) should_== s"stuff $expectedFoo"
    }

    "Map with possible default" in {
      val req = Request().putHeaders(etag, lenheader)

      val c1 = captureMapR(headers.`Content-Length`)(r => Right(r.length))
      RuleExecutor.runRequestRules(c1.rule, req) should_== SuccessResponse(4::HNil)

      val r2 = Gone("Foo")
      val c2 = captureMapR(headers.`Content-Length`)(_ => Left(r2))
      val v1 = RuleExecutor.runRequestRules(c2.rule, req)
      v1 must beAnInstanceOf[FailureResponse]
      v1.asInstanceOf[FailureResponse].toResponse.unsafeRun.status must_== r2.unsafeRun.resp.status

      val c3 = captureMapR(headers.`Access-Control-Allow-Credentials`, Some(r2))(_ => ???)
      val v2 = RuleExecutor.runRequestRules(c3.rule, req)
      v2 must beAnInstanceOf[FailureResponse]
      v2.asInstanceOf[FailureResponse].toResponse.unsafeRun.status must_== r2.unsafeRun.resp.status
    }

    "Append headers to a Route" in {

      val path = POST / "hello" / 'world +? param[Int]("fav")
      val validations = existsAnd(headers.`Content-Length`){ h => h.length != 0 }


      val route = runWith((path >>> validations >>> capture(ETag)).decoding(EntityDecoder.text)) {
        (world: String, fav: Int, tag: ETag, body: String) =>
          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .putHeaders(tag)
        }

      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").right.getOrElse(sys.error("Fail")))
        .putHeaders(etag)
        .withBody("cool")
        .unsafeRun

      val resp = route(req).unsafeRun.orNotFound
      resp.headers.get(ETag) must beSome(etag)

    }

    "accept compound or sequential header rules" in {

      val path = POST / "hello" / 'world
      val lplus1 = captureMap(headers.`Content-Length`)(_.length + 1)


      val route1 = runWith((path >>> lplus1 >>> capture(ETag)).decoding(EntityDecoder.text)) {
        (world: String, lplus1: Long, tag: ETag, body: String) =>
          Ok("")
      }

      val route2 = runWith((path >>> (lplus1 && capture(ETag))).decoding(EntityDecoder.text)) {
        (world: String, _: Long, tag: ETag, body: String) =>
          Ok("")
      }

      val body = Stream.emit(ByteVector("cool".getBytes))
      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").right.getOrElse(sys.error("Fail")))
        .putHeaders(ETag(ETag.EntityTag("foo")))
        .withBody("cool")
        .unsafeRun

      route1(req).unsafeRun.orNotFound.status should_== Status.Ok
      route2(req).unsafeRun.orNotFound.status should_== Status.Ok

    }

    "Run || routes" in {
      val p1 = "one" / 'two
      val p2 = "three" / 'four

      val f = runWith(GET / (p1 || p2)) { (s: String) => Ok("").putHeaders(ETag(ETag.EntityTag(s))) }

      val req1 = Request(uri = Uri.fromString("/one/two").right.getOrElse(sys.error("Failed.")))
      checkETag(f(req1), "two")

      val req2 = Request(uri = Uri.fromString("/three/four").right.getOrElse(sys.error("Failed.")))
      checkETag(f(req2), "four")
    }

    "Execute a complicated route" in {

      val path = POST / "hello" / 'world +? param[Int]("fav")
      val validations = existsAnd(headers.`Content-Length`){ h => h.length != 0 } &&
        capture(ETag)

      val route =
        runWith((path >>> validations).decoding(EntityDecoder.text)) {(world: String, fav: Int, tag: ETag, body: String) =>

          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .putHeaders(ETag(ETag.EntityTag("foo")))
        }

      val req = Request(POST, uri = Uri.fromString("/hello/neptune?fav=23").right.getOrElse(sys.error("Fail")))
        .putHeaders( ETag(ETag.EntityTag("foo")))
        .withBody("cool")
        .unsafeRun()

      checkETag(route(req), "foo")
    }

    "Deal with 'no entity' responses" in {
      val route = runWith(GET / "foo") { () => SwitchingProtocols() }
      val req = Request(GET, uri = Uri.fromString("/foo").right.getOrElse(sys.error("Fail")))

      val result = route(req).unsafeRun.orNotFound
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



    "traverse a captureless path" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello").right.getOrElse(sys.error("Failed.")))

      val f = runWith(stuff) { () => Ok("Cool.").putHeaders(ETag(ETag.EntityTag("foo"))) }
      checkETag(f(req), "foo")
    }

    "Not match a path to long" in {
      val stuff = GET / "hello"
      val req = Request(uri = uri("/hello/world"))

      val f = runWith(stuff) { () => Ok("Shouldn't get here.") }
      val r = f(req).unsafeRun.orNotFound
      r.status should_== Status.NotFound
    }

    "capture a variable" in {
      val stuff = GET / 'hello
      val req = Request(uri = Uri.fromString("/hello").right.getOrElse(sys.error("Failed.")))

      val f = runWith(stuff) { str: String => Ok("Cool.").putHeaders(ETag(ETag.EntityTag(str))) }
      checkETag(f(req), "hello")
    }

    "work directly" in {
      val stuff = GET / "hello"
      val req = Request(uri = Uri.fromString("/hello").right.getOrElse(sys.error("Failed.")))

      val f = runWith(stuff) { () => Ok("Cool.").putHeaders(ETag(ETag.EntityTag("foo"))) }

      checkETag(f(req), "foo")
    }

    "capture end with nothing" in {
      val stuff = GET / "hello" / *
      val req = Request(uri = Uri.fromString("/hello").right.getOrElse(sys.error("Failed.")))
      val f = runWith(stuff) { path: List[String] => Ok("Cool.").putHeaders(ETag(ETag.EntityTag(if (path.isEmpty) "go" else "nogo"))) }

      checkETag(f(req), "go")
    }

    "capture remaining" in {
      val stuff = GET / "hello" / *
      val req = Request(uri = Uri.fromString("/hello/world/foo").right.getOrElse(sys.error("Failed.")))
      val f = runWith(stuff) { path: List[String] => Ok("Cool.").putHeaders(ETag(ETag.EntityTag(path.mkString))) }

      checkETag(f(req), "worldfoo")
    }
  }

  "Query validators" should {
    "get a query string" in {
      val path = GET / "hello" +? param[Int]("jimbo")
      val req = Request(uri = Uri.fromString("/hello?jimbo=32").right.getOrElse(sys.error("Failed.")))

      val route = runWith(path) { i: Int => Ok("stuff").putHeaders(ETag(ETag.EntityTag((i + 1).toString))) }

      checkETag(route(req), "33")
    }

    "accept compound or sequential query rules" in {
      val path = GET / "hello"

      val req = Request(uri = uri("/hello?foo=bar&baz=1"))

      val route1 = runWith(path +? param[String]("foo") & param[Int]("baz")) { (_: String, _: Int) =>
        Ok("")
      }

      route1(req).unsafeRun.orNotFound.status should_== Status.Ok

      val route2 = runWith(path +? (param[String]("foo") and param[Int]("baz"))) { (_: String, _: Int) =>
        Ok("")
      }

      route2(req).unsafeRun.orNotFound.status should_== Status.Ok
    }

    "map simple query rules into a complex type" in {
      case class Foo(i: Int, f: Double, s: String)
      val paramFoo = param[Int]("i") & param[Double]("f") & param[String]("s") map Foo.apply _

      val path = GET / "hello" +? paramFoo
      val req = Request(uri = Uri.fromString("/hello?i=32&f=3.2&s=Asdf").right.getOrElse(sys.error("Failed.")))

      val route = runWith(path) { (f: Foo) => Ok(s"stuff $f") }

      val result = route(req).unsafeRun.orNotFound
      result.status should_== Status.Ok
      RequestRunner.getBody(result.body) should_== "stuff Foo(32,3.2,Asdf)"
    }
  }

  "Decoders" should {
    "Decode a body" in {
      val reqHeader = existsAnd(headers.`Content-Length`){ h => h.length < 10 }

      val path = POST / "hello" >>> reqHeader


      val req1 = Request(POST, uri = Uri.fromString("/hello").right.getOrElse(sys.error("Fail")))
                    .withBody("foo")
                    .unsafeRun

      val req2 = Request(POST, uri = Uri.fromString("/hello").right.getOrElse(sys.error("Fail")))
        .withBody("0123456789") // length 10
        .unsafeRun

      val route = runWith(path.decoding(EntityDecoder.text)) { str: String =>
        Ok("stuff").putHeaders(ETag(ETag.EntityTag(str)))
      }

      checkETag(route(req1), "foo")
      route(req2).unsafeRun.orNotFound.status should_== Status.BadRequest
    }

    "Allow the infix operator syntax" in {
      val path = POST / "hello"

      val req = Request(POST, uri = Uri.fromString("/hello").right.getOrElse(sys.error("Fail")))
        .withBody("foo")
        .unsafeRun

      val route = runWith(path ^ EntityDecoder.text) { str: String =>
        Ok("stuff").putHeaders(ETag(ETag.EntityTag(str)))
      }

      checkETag(route(req), "foo")
    }

    "Fail on a header" in {
      val path = GET / "hello"

      val req = Request(uri = uri("/hello"))
                  .putHeaders(headers.`Content-Length`("foo".length))

      val reqHeader = existsAnd(headers.`Content-Length`){ h => h.length < 2}
      val route1 = runWith(path.validate(reqHeader)) { () =>
        Ok("shouldn't get here.")
      }

      route1(req).unsafeRun.orNotFound.status should_== Status.BadRequest

      val reqHeaderR = existsAndR(headers.`Content-Length`){ h => Some(Unauthorized("Foo."))}
      val route2 = runWith(path.validate(reqHeaderR)) { () =>
        Ok("shouldn't get here.")
      }

      route2(req).unsafeRun.orNotFound.status should_== Status.Unauthorized
    }

    "Fail on a query" in {
      val path = GET / "hello"

      val req = Request(uri = uri("/hello?foo=bar"))
                  .putHeaders(`Content-Length`("foo".length))

      val route1 = runWith(path +? param[Int]("foo")) { i: Int =>
        Ok("shouldn't get here.")
      }

      route1(req).unsafeRun.orNotFound.status should_== Status.BadRequest

      val route2 = runWith(path +? paramR[String]("foo", (_: String) => Some(Unauthorized("foo")))) { str: String =>
        Ok("shouldn't get here.")
      }

      route2(req).unsafeRun.orNotFound.status should_== Status.Unauthorized
    }
  }

  "Path prepending" should {

    val req = Request(uri=uri("/foo/bar"))
    val respMsg = "Result"

    "Work for a PathBuilder" in {
      val tail = GET / "bar"
      val all = "foo" /: tail
      runWith(all)(respMsg).apply(req).unsafeRun.orNotFound.as[String].unsafeRun === respMsg
    }

    "Work for a QueryBuilder" in {
      val tail = GET / "bar" +? param[String]("str")
      val all = "foo" /: tail
      runWith(all){ q: String => respMsg + q}
        .apply(req.copy(uri=uri("/foo/bar?str=answer")))
        .unsafeRun.orNotFound.as[String].unsafeRun === respMsg + "answer"
    }

    "Work for a Router" in {
      val tail = GET / "bar" >>> RequireETag
      val all = "foo" /: tail
      runWith(all)(respMsg)
        .apply(req.copy(uri=uri("/foo/bar")).putHeaders(etag))
        .unsafeRun.orNotFound.as[String].unsafeRun === respMsg
    }
  }
}
