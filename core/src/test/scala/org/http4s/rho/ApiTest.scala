package org.http4s
package rho

import scala.collection.immutable.Seq
import cats.data.OptionT
import cats.effect.{IO, Sync}
import org.http4s.headers.{ETag, `Content-Length`}
import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.bits.RequestAST.AndRule
import org.http4s.rho.bits._
import org.http4s.rho.io._
import org.http4s.Uri.uri
import org.specs2.matcher.MatchResult
import org.specs2.mutable._
import shapeless.{HList, HNil}

class ApiTest extends Specification {

  object ruleExecutor extends RuleExecutor[IO]

  def runWith[F[_]: Sync, T <: HList, FU](exec: RouteExecutable[F, T])(f: FU)(implicit fpm: FuncParamsMatch[F, T, FU], frm: FuncResultMatch[F, T, FU], hltf: HListToFunc[F, T, FU]): Request[F] => OptionT[F, Response[F]] = {
    val srvc = new RhoRoutes[F] { exec |>> f }.toRoutes()
    srvc.apply(_: Request[F])
  }

  val lenheader: `Content-Length` =
    headers.`Content-Length`.unsafeFromLong(4)

  val etag: ETag =
    ETag(ETag.EntityTag("foo"))

  val RequireETag =
    exists(ETag)

  val RequireNonZeroLen =
    existsAnd(headers.`Content-Length`){ h => h.length != 0 }

  val RequireThrowException =
    existsAnd(headers.`Content-Length`){ _ => throw new RuntimeException("this could happen") }

  def fetchETag(p: IO[Response[IO]]): ETag = {
    val resp = p.unsafeRunSync()
    resp.headers.get(ETag).getOrElse(sys.error("No ETag: " + resp))
  }

  def checkETag(p: OptionT[IO, Response[IO]], s: String): MatchResult[Any] =
    fetchETag(p.value.map(_.getOrElse(Response.notFound))) must_== ETag(ETag.EntityTag(s))

  "RhoDsl bits" should {
    "Combine validators" in {
      (RequireETag && RequireNonZeroLen) should_== TypedHeader(AndRule(RequireETag.rule, RequireNonZeroLen.rule))
    }

    "Fail on a bad request" in {
      val badreq = Request[IO]().putHeaders(lenheader)
      val res = ruleExecutor.runRequestRules((RequireETag && RequireNonZeroLen).rule, badreq)

      res must beAnInstanceOf[FailureResponse[IO]]
      res.asInstanceOf[FailureResponse[IO]].toResponse.unsafeRunSync().status must_== Status.BadRequest
    }

    "Fail on a bad request 2" in {
      val req = Request[IO]().putHeaders(lenheader)
      val res = ruleExecutor.runRequestRules(RequireThrowException.rule, req)

      res must beAnInstanceOf[FailureResponse[IO]]
      res.asInstanceOf[FailureResponse[IO]].toResponse.unsafeRunSync().status must_== Status.InternalServerError
    }

    "Match captureless route" in {
      val c = RequireETag && RequireNonZeroLen

      val req = Request[IO]().putHeaders(etag, lenheader)
      ruleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(HNil)
    }

    "optionally capture header params" in {
      val req = Request[IO]().putHeaders(lenheader)

      Seq({
        val c = captureOptionally(headers.`Content-Length`)
        ruleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(Some(lenheader) :: HNil)
      }, {
        val c = captureOptionally(ETag)
        ruleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(None :: HNil)
      }).reduce(_ and _)
    }

    "Capture params" in {
      val req = Request[IO]().putHeaders(etag, lenheader)
      Seq({
        val c2 = RequireETag && capture(headers.`Content-Length`)
        ruleExecutor.runRequestRules(c2.rule, req) should_== SuccessResponse(lenheader :: HNil)
      }, {
        val c3 = capture(headers.`Content-Length`) && capture(ETag)
        ruleExecutor.runRequestRules(c3.rule, req) should_== SuccessResponse(etag :: lenheader :: HNil)
      }).reduce(_ and _)
    }

    "Capture params with default" in {
      val default: `Content-Length` = headers.`Content-Length`.unsafeFromLong(10)
      val c = captureOrElse(headers.`Content-Length`)(default)
      Seq({
        val req = Request[IO]().putHeaders()
        ruleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(default :: HNil)
      }, {
        val req = Request[IO]().putHeaders(lenheader)
        ruleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(lenheader :: HNil)
      }).reduce(_ and _)
    }

    "Map header params" in {
      val req = Request[IO]().putHeaders(etag, lenheader)
      val c = captureMap(headers.`Content-Length`)(_.length)

      ruleExecutor.runRequestRules(c.rule, req) should_== SuccessResponse(4 :: HNil)
    }

    "Map header params with exception" in {
      val req = Request[IO]().putHeaders(etag, lenheader)
      val c = captureMap(headers.`Content-Length`)(_.length / 0)

      ruleExecutor.runRequestRules(c.rule, req) must beAnInstanceOf[FailureResponse[IO]]
    }


    "map simple header params into a complex type" in {
      case class Foo(age: Long, s: HttpDate)
      val paramFoo = captureMap(headers.`Content-Length`)(_.length) && captureMap(headers.Date)(_.date) map Foo.apply _

      val path = GET / "hello" >>> paramFoo
      val testDate = HttpDate.unsafeFromInstant(java.time.Instant.now)
      val req = Request[IO](
        uri = Uri.fromString("/hello?i=32&f=3.2&s=Asdf").getOrElse(sys.error("Failed.")),
        headers = Headers.of(headers.`Content-Length`.unsafeFromLong(10), headers.Date(testDate))
      )

      val expectedFoo = Foo(10, testDate)
      val route = runWith(path) { (f: Foo) => Ok(s"stuff $f") }

      val result = route(req).value.unsafeRunSync().getOrElse(Response.notFound)
      result.status should_== Status.Ok
      RequestRunner.getBody(result.body) should_== s"stuff $expectedFoo"
    }

    "Map with missing header result" in {
      val req = Request[IO]().putHeaders(etag, lenheader)

      val c1 = captureMapR(headers.`Content-Length`)(r => Right(r.length))
      ruleExecutor.runRequestRules(c1.rule, req) should_== SuccessResponse(4::HNil)

      val r2 = Gone("Foo")
      val c2 = captureMapR(headers.`Content-Length`)(_ => Left(r2))
      val v1 = ruleExecutor.runRequestRules(c2.rule, req)
      v1 must beAnInstanceOf[FailureResponse[IO]]
      v1.asInstanceOf[FailureResponse[IO]].toResponse.unsafeRunSync().status must_== r2.unsafeRunSync().resp.status

      val c3 = captureMapR(headers.`Access-Control-Allow-Credentials`, Some(r2))(_ => ???)
      val v2 = ruleExecutor.runRequestRules(c3.rule, req)
      v2 must beAnInstanceOf[FailureResponse[IO]]
      v2.asInstanceOf[FailureResponse[IO]].toResponse.unsafeRunSync().status must_== r2.unsafeRunSync().resp.status
    }

    "Append headers to a Route" in {
      val path = POST / "hello" / pv"world" +? param[Int]("fav")
      val validations = existsAnd(headers.`Content-Length`){ h => h.length != 0 }

      val route = runWith((path >>> validations >>> capture(ETag)).decoding(EntityDecoder.text[IO])) {
        (world: String, fav: Int, tag: ETag, body: String) =>
          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .map(_.putHeaders(tag))
        }

      val req = Request[IO](POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders(etag)
        .withEntity("cool")

      val resp = route(req).value.unsafeRunSync().getOrElse(Response.notFound)
      resp.headers.get(ETag) must beSome(etag)

    }

    "accept compound or sequential header rules" in {
      val path = POST / "hello" / pv"world"
      val lplus1 = captureMap(headers.`Content-Length`)(_.length + 1)

      val route1 = runWith((path >>> lplus1 >>> capture(ETag)).decoding(EntityDecoder.text)) {
        (_: String, _: Long, _: ETag, _: String) =>
          Ok("")
      }

      val route2 = runWith((path >>> (lplus1 && capture(ETag))).decoding(EntityDecoder.text)) {
        (_: String, _: Long, _: ETag, _: String) =>
          Ok("")
      }

      val req = Request[IO](POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders(ETag(ETag.EntityTag("foo")))
        .withEntity("cool")

      route1(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.Ok
      route2(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.Ok
    }

    "Run || routes" in {
      val p1 = "one" / pv"two"
      val p2 = "three" / pv"four"

      val f = runWith(GET / (p1 || p2)) { (s: String) => Ok("").map(_.putHeaders(ETag(ETag.EntityTag(s)))) }

      val req1 = Request[IO](uri = Uri.fromString("/one/two").getOrElse(sys.error("Failed.")))
      checkETag(f(req1), "two")

      val req2 = Request[IO](uri = Uri.fromString("/three/four").getOrElse(sys.error("Failed.")))
      checkETag(f(req2), "four")
    }

    "Execute a complicated route" in {

      val path = POST / "hello" / pv"world" +? param[Int]("fav")
      val validations = existsAnd(headers.`Content-Length`){ h => h.length != 0 } &&
        capture(ETag)

      val route =
        runWith((path >>> validations).decoding(EntityDecoder.text)) {(world: String, fav: Int, _: ETag, body: String) =>

          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .map(_.putHeaders(ETag(ETag.EntityTag("foo"))))
        }

      val req = Request[IO](POST, uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail")))
        .putHeaders( ETag(ETag.EntityTag("foo")))
        .withEntity("cool")

      checkETag(route(req), "foo")
    }

    "Deal with 'no entity' responses" in {
      val route = runWith(GET / "foo") { () => SwitchingProtocols.apply }
      val req = Request[IO](GET, uri = Uri.fromString("/foo").getOrElse(sys.error("Fail")))

      val result = route(req).value.unsafeRunSync().getOrElse(Response.notFound)
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
      (pathMatch("hello") / requestLine).isInstanceOf[RequestLineBuilder[IO, _]] should_== true
    }
  }

  "PathValidator" should {

    "traverse a captureless path" in {
      val stuff = GET / "hello"
      val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = runWith(stuff) { () => Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag("foo")))) }
      checkETag(f(req), "foo")
    }

    "Not match a path to long" in {
      val stuff = GET / "hello"
      val req = Request[IO](uri = uri("/hello/world"))

      val f = runWith(stuff) { () => Ok("Shouldn't get here.") }
      val r = f(req).value.unsafeRunSync().getOrElse(Response.notFound)
      r.status should_== Status.NotFound
    }

    "capture a variable" in {
      val stuff = GET / pv"hello"
      val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = runWith(stuff) { str: String => Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag(str)))) }
      checkETag(f(req), "hello")
    }

    "work directly" in {
      val stuff = GET / "hello"
      val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

      val f = runWith(stuff) { () =>
        Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag("foo"))))
      }

      checkETag(f(req), "foo")
    }

    "capture end with nothing" in {
      val stuff = GET / "hello" / *
      val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))
      val f = runWith(stuff) { path: List[String] =>
        Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag(if (path.isEmpty) "go" else "nogo"))))
      }

      checkETag(f(req), "go")
    }

    "capture remaining" in {
      val stuff = GET / "hello" / *
      val req = Request[IO](uri = Uri.fromString("/hello/world/foo").getOrElse(sys.error("Failed.")))
      val f = runWith(stuff) { path: List[String] => Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag(path.mkString)))) }

      checkETag(f(req), "worldfoo")
    }
  }

  "Query validators" should {
    "get a query string" in {
      val path = GET / "hello" +? param[Int]("jimbo")
      val req = Request[IO](uri = Uri.fromString("/hello?jimbo=32").getOrElse(sys.error("Failed.")))

      val route = runWith(path) { i: Int => Ok("stuff").map(_.putHeaders(ETag(ETag.EntityTag((i + 1).toString)))) }

      checkETag(route(req), "33")
    }

    "accept compound or sequential query rules" in {
      val path = GET / "hello"

      val req = Request[IO](uri = uri("/hello?foo=bar&baz=1"))

      val route1 = runWith(path +? param[String]("foo") & param[Int]("baz")) { (_: String, _: Int) =>
        Ok("")
      }

      route1(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.Ok

      val route2 = runWith(path +? (param[String]("foo") and param[Int]("baz"))) { (_: String, _: Int) =>
        Ok("")
      }

      route2(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.Ok
    }

    "map simple query rules into a complex type" in {
      case class Foo(i: Int, f: Double, s: String)
      val paramFoo = param[Int]("i") & param[Double]("f") & param[String]("s") map Foo.apply _

      val path = GET / "hello" +? paramFoo
      val req = Request[IO](uri = Uri.fromString("/hello?i=32&f=3.2&s=Asdf").getOrElse(sys.error("Failed.")))

      val route = runWith(path) { (f: Foo) => Ok(s"stuff $f") }

      val result = route(req).value.unsafeRunSync().getOrElse(Response.notFound)
      result.status should_== Status.Ok
      RequestRunner.getBody(result.body) should_== "stuff Foo(32,3.2,Asdf)"
    }
  }

  "Decoders" should {
    "Decode a body" in {
      val reqHeader = existsAnd(headers.`Content-Length`){ h => h.length < 10 }

      val path = POST / "hello" >>> reqHeader


      val req1 = Request[IO](POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
                    .withEntity("foo")

      val req2 = Request[IO](POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
        .withEntity("0123456789") // length 10

      val route = runWith(path.decoding(EntityDecoder.text)) { str: String =>
        Ok("stuff").map(_.putHeaders(ETag(ETag.EntityTag(str))))
      }

      checkETag(route(req1), "foo")
      route(req2).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.BadRequest
    }

    "Allow the infix operator syntax" in {
      val path = POST / "hello"

      val req = Request[IO](POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
        .withEntity("foo")

      val route = runWith(path ^ EntityDecoder.text) { str: String =>
        Ok("stuff").map(_.putHeaders(ETag(ETag.EntityTag(str))))
      }

      checkETag(route(req), "foo")
    }

    "Fail on a header" in {
      val path = GET / "hello"

      val req = Request[IO](uri = uri("/hello"))
                  .putHeaders(headers.`Content-Length`.unsafeFromLong("foo".length))

      val reqHeader = existsAnd(headers.`Content-Length`){ h => h.length < 2}
      val route1 = runWith(path.validate(reqHeader)) { () =>
        Ok("shouldn't get here.")
      }

      route1(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.BadRequest

      val reqHeaderR = existsAndR(headers.`Content-Length`){ _ => Some(Unauthorized("Foo."))}
      val route2 = runWith(path.validate(reqHeaderR)) { () =>
        Ok("shouldn't get here.")
      }

      route2(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.Unauthorized
    }

    "Fail on a query" in {
      val path = GET / "hello"

      val req = Request[IO](uri = uri("/hello?foo=bar"))
                  .putHeaders(`Content-Length`.unsafeFromLong("foo".length))

      val route1 = runWith(path +? param[Int]("foo")) { _: Int =>
        Ok("shouldn't get here.")
      }

      route1(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.BadRequest

      val route2 = runWith(path +? paramR[String]("foo", (_: String) => Some(Unauthorized("foo")))) { _: String =>
        Ok("shouldn't get here.")
      }

      route2(req).value.unsafeRunSync().getOrElse(Response.notFound).status should_== Status.Unauthorized
    }
  }

  "Path prepending" should {
    val req = Request[IO](uri=uri("/foo/bar"))
    val respMsg = "Result"

    "Work for a PathBuilder" in {
      val tail = GET / "bar"
      val all = "foo" /: tail

      runWith(all)(respMsg).apply(req).value.unsafeRunSync().getOrElse(Response.notFound).as[String].unsafeRunSync() === respMsg
    }

    "Work for a QueryBuilder" in {
      val tail = GET / "bar" +? param[String]("str")
      val all = "foo" /: tail

      runWith(all){ q: String => respMsg + q}
        .apply(req.withUri(uri("/foo/bar?str=answer")))
        .value
        .unsafeRunSync()
        .getOrElse(Response.notFound).as[String].unsafeRunSync() === respMsg + "answer"
    }

    "Work for a Router" in {
      val tail = GET / "bar" >>> RequireETag
      val all = "foo" /: tail

      runWith(all)(respMsg)
        .apply(req.withUri(uri("/foo/bar")).putHeaders(etag))
        .value.unsafeRunSync().getOrElse(Response.notFound).as[String].unsafeRunSync() === respMsg
    }
  }
}
