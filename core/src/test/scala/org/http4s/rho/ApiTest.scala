package org.http4s
package rho

import cats.data.OptionT
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import munit.FunSuite
import org.http4s.headers.Accept
import org.http4s.headers.{ETag, `Content-Length`}
import org.http4s.rho.bits._
import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.bits.RequestAST.AndRule
import org.http4s.rho.io._
import shapeless.{HList, HNil}

import scala.util.control.NoStackTrace

class ApiTest extends FunSuite {

  private object ruleExecutor extends RuleExecutor[IO]

  private def runWith[F[_]: Sync, T <: HList, FU](exec: RouteExecutable[F, T])(f: FU)(implicit
      fpm: FuncParamsMatch[F, T, FU],
      hltf: HListToFunc[F, T, FU]): Request[F] => OptionT[F, Response[F]] = {
    val srvc = new RhoRoutes[F] { exec |>> f }.toRoutes()
    srvc.apply(_: Request[F])
  }

  private val lenheader: `Content-Length` =
    headers.`Content-Length`.unsafeFromLong(4)

  private val etag: ETag =
    ETag(ETag.EntityTag("foo"))

  private val RequireETag =
    H[ETag].exists

  private val RequireNonZeroLen =
    H[`Content-Length`].existsAnd(h => h.length != 0)

  private val RequireThrowException =
    H[`Content-Length`].existsAnd(_ =>
      throw new RuntimeException("this could happen") with NoStackTrace
    )

  private def fetchETag(p: IO[Response[IO]]): ETag = {
    val resp = p.unsafeRunSync()
    resp.headers.get[ETag].getOrElse(sys.error("No ETag: " + resp))
  }

  private def checkETag(p: OptionT[IO, Response[IO]], s: String) =
    assertEquals(fetchETag(p.value.map(_.getOrElse(Response.notFound))), ETag(ETag.EntityTag(s)))

  test("A RhoDsl bits should combine validators") {
    assertEquals(
      RequireETag && RequireNonZeroLen,
      TypedHeader[IO, HNil](
        AndRule(RequireETag.rule, RequireNonZeroLen.rule)
      )
    )
  }

  test("A RhoDsl bits should fail on a bad request") {
    val badreq = Request[IO]().putHeaders(lenheader)
    val res = ruleExecutor.runRequestRules((RequireETag && RequireNonZeroLen).rule, badreq)

    res match {
      case resp: FailureResponse[IO] =>
        assertEquals(resp.toResponse.unsafeRunSync().status, Status.BadRequest)

      case _ =>
        fail("Unexpected putHeaders result found")
    }
  }

  test("A RhoDsl bits should fail on a bad request 2") {
    val req = Request[IO]().putHeaders(lenheader)
    val res = ruleExecutor.runRequestRules(RequireThrowException.rule, req)

    res match {
      case resp: FailureResponse[IO] =>
        assertEquals(resp.toResponse.unsafeRunSync().status, Status.InternalServerError)

      case _ =>
        fail("Unexpected putHeaders result found")
    }
  }

  test("A RhoDsl bits should match captureless route") {
    val c = RequireETag && RequireNonZeroLen

    val req = Request[IO]().putHeaders(etag, lenheader)
    assertEquals(ruleExecutor.runRequestRules(c.rule, req), SuccessResponse[IO, HNil](HNil))
  }

  test("A RhoDsl bits should optionally capture header params") {
    val req = Request[IO]().putHeaders(lenheader)

    val c = H[`Content-Length`].captureOptionally

    assertEquals(
      ruleExecutor.runRequestRules(c.rule, req),
      SuccessResponse[IO, HList](
        Some(lenheader) :: HNil
      )
    )

    val cETag = H[ETag].captureOptionally

    assertEquals(
      ruleExecutor.runRequestRules(cETag.rule, req),
      SuccessResponse[IO, HList](None :: HNil)
    )
  }

  test("A RhoDsl bits should capture params") {
    val req = Request[IO]().putHeaders(etag, lenheader)

    val c2 = RequireETag && H[`Content-Length`].capture
    assertEquals(
      ruleExecutor.runRequestRules(c2.rule, req),
      SuccessResponse[IO, HList](lenheader :: HNil)
    )

    val c3 = H[`Content-Length`].capture && H[ETag].capture
    assertEquals(
      ruleExecutor.runRequestRules(c3.rule, req),
      SuccessResponse[IO, HList](
        etag :: lenheader :: HNil
      )
    )
  }

  test("A RhoDsl bits should capture params with default") {
    val default: `Content-Length` = headers.`Content-Length`.unsafeFromLong(10)
    val c = H[`Content-Length`].captureOrElse(default)

    val req1 = Request[IO]().putHeaders()
    assertEquals(
      ruleExecutor.runRequestRules(c.rule, req1),
      SuccessResponse[IO, HList](default :: HNil)
    )

    val req2 = Request[IO]().putHeaders(lenheader)
    assertEquals(
      ruleExecutor.runRequestRules(c.rule, req2),
      SuccessResponse[IO, HList](lenheader :: HNil)
    )
  }

  test("A RhoDsl bits should map header params") {
    val req = Request[IO]().putHeaders(etag, lenheader)
    val c = H[`Content-Length`].captureMap(_.length)

    assertEquals(ruleExecutor.runRequestRules(c.rule, req), SuccessResponse[IO, HList](4 :: HNil))
  }

  test("A RhoDsl bits should map header params with exception") {
    val req = Request[IO]().putHeaders(etag, lenheader)
    val c = H[`Content-Length`].captureMap(_.length / 0)

    ruleExecutor.runRequestRules(c.rule, req) match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected putHeaders result found")
    }
  }

  test("A RhoDsl bits should map simple header params into a complex type") {
    case class Foo(age: Long, s: HttpDate)
    val paramFoo =
      (H[`Content-Length`].captureMap(_.length) && H[headers.Date].captureMap(_.date))
        .map(Foo.apply _)

    val path = GET / "hello" >>> paramFoo
    val testDate = HttpDate.unsafeFromInstant(java.time.Instant.now)
    val req = Request[IO](
      uri = Uri.fromString("/hello?i=32&f=3.2&s=Asdf").getOrElse(sys.error("Failed.")),
      headers = Headers(headers.`Content-Length`.unsafeFromLong(10), headers.Date(testDate))
    )

    val expectedFoo = Foo(10, testDate)
    val route = runWith(path)((f: Foo) => Ok(s"stuff $f"))

    val result = route(req).value.unsafeRunSync().getOrElse(Response.notFound)
    assertEquals(result.status, Status.Ok)
    assertEquals(RequestRunner.getBody(result.body), s"stuff $expectedFoo")
  }

  test("A RhoDsl bits should map with missing header result") {
    val req = Request[IO]().putHeaders(etag, lenheader)

    val c1 = H[`Content-Length`].captureMapR(r => Right(r.length))
    assertEquals(ruleExecutor.runRequestRules(c1.rule, req), SuccessResponse[IO, HList](4 :: HNil))

    val r2 = Gone("Foo")
    val c2 = H[`Content-Length`].captureMapR(_ => Left(r2))
    val v1 = ruleExecutor.runRequestRules(c2.rule, req)

    v1 match {
      case r: FailureResponse[IO] =>
        assertEquals(
          r.toResponse.unsafeRunSync().status,
          r2
            .unsafeRunSync()
            .resp
            .status
        )

      case _ =>
        fail("Unexpected runRequestRules result found")
    }

    val c3 = H[Accept].captureMapR(Some(r2))(_ => ???)
    val v2 = ruleExecutor.runRequestRules(c3.rule, req)

    v2 match {
      case r: FailureResponse[IO] =>
        assertEquals(
          r.toResponse.unsafeRunSync().status,
          r2
            .unsafeRunSync()
            .resp
            .status
        )

      case _ =>
        fail("Unexpected runRequestRules result found")
    }
  }

  test("A RhoDsl bits should append headers to a Route") {
    val path = POST / "hello" / pv"world" +? param[Int]("fav")
    val validations = H[`Content-Length`].existsAnd(h => h.length != 0)

    val route =
      runWith((path >>> validations >>> H[ETag].capture).decoding(EntityDecoder.text[IO])) {
        (world: String, fav: Int, tag: ETag, body: String) =>
          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .map(_.putHeaders(tag))
      }

    val req = Request[IO](
      POST,
      uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail"))
    )
      .putHeaders(etag)
      .withEntity("cool")

    val resp = route(req).value.unsafeRunSync().getOrElse(Response.notFound)

    assertEquals(resp.headers.get[ETag], Some(etag))
  }

  test("A RhoDsl bits should accept compound or sequential header rules") {
    val path = POST / "hello" / pv"world"
    val lplus1 = H[`Content-Length`].captureMap(_.length + 1)

    val route1 = runWith((path >>> lplus1 >>> H[ETag].capture).decoding(EntityDecoder.text)) {
      (_: String, _: Long, _: ETag, _: String) =>
        Ok("")
    }

    val route2 = runWith((path >>> (lplus1 && H[ETag].capture)).decoding(EntityDecoder.text)) {
      (_: String, _: Long, _: ETag, _: String) =>
        Ok("")
    }

    val req = Request[IO](
      POST,
      uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail"))
    )
      .putHeaders(ETag(ETag.EntityTag("foo")))
      .withEntity("cool")

    assertEquals(route1(req).value.unsafeRunSync().getOrElse(Response.notFound).status, Status.Ok)
    assertEquals(route2(req).value.unsafeRunSync().getOrElse(Response.notFound).status, Status.Ok)
  }

  test("A RhoDsl bits should run || routes") {
    val p1 = "one" / pv"two"
    val p2 = "three" / pv"four"

    val f = runWith(GET / (p1 || p2)) { (s: String) =>
      Ok("").map(_.putHeaders(ETag(ETag.EntityTag(s))))
    }

    val req1 = Request[IO](uri = Uri.fromString("/one/two").getOrElse(sys.error("Failed.")))
    checkETag(f(req1), "two")

    val req2 = Request[IO](uri = Uri.fromString("/three/four").getOrElse(sys.error("Failed.")))
    checkETag(f(req2), "four")
  }

  test("A RhoDsl bits should execute a complicated route") {
    val path = POST / "hello" / pv"world" +? param[Int]("fav")
    val validations = H[`Content-Length`].existsAnd(h => h.length != 0) &&
      H[ETag].capture

    val route =
      runWith((path >>> validations).decoding(EntityDecoder.text)) {
        (world: String, fav: Int, _: ETag, body: String) =>
          Ok(s"Hello to you too, $world. Your Fav number is $fav. You sent me $body")
            .map(_.putHeaders(ETag(ETag.EntityTag("foo"))))
      }

    val req = Request[IO](
      POST,
      uri = Uri.fromString("/hello/neptune?fav=23").getOrElse(sys.error("Fail"))
    )
      .putHeaders(ETag(ETag.EntityTag("foo")))
      .withEntity("cool")

    checkETag(route(req), "foo")
  }

  test("A RhoDsl bits should deal with 'no entity' responses") {
    val route = runWith(GET / "foo")(() => SwitchingProtocols.apply)
    val req = Request[IO](GET, uri = Uri.fromString("/foo").getOrElse(sys.error("Fail")))

    val result = route(req).value.unsafeRunSync().getOrElse(Response.notFound)

    assertEquals(result.headers.headers.size, 0)

    assertEquals(result.status, Status.SwitchingProtocols)
  }

  test("A RequestLineBuilder should be made from TypedPath and TypedQuery") {
    val path = pathMatch("foo")
    val q = param[Int]("bar")

    assertEquals(path +? q, RequestLineBuilder[IO, shapeless.::[Int, HNil]](path.rule, q.rule))
  }

  test("A RequestLineBuilder should append to a TypedPath") {
    val requestLine = pathMatch("foo") +? param[Int]("bar")
    assert((pathMatch("hello") / requestLine).isInstanceOf[RequestLineBuilder[IO, _]])
  }

  test("A PathValidator should traverse a captureless path") {
    val stuff = GET / "hello"
    val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

    val f = runWith(stuff)(() => Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag("foo")))))
    checkETag(f(req), "foo")
  }

  test("A PathValidator should not match a path to long") {
    val stuff = GET / "hello"
    val req = Request[IO](uri = uri"/hello/world")

    val f = runWith(stuff)(() => Ok("Shouldn't get here."))
    val r = f(req).value.unsafeRunSync().getOrElse(Response.notFound)
    assertEquals(r.status, Status.NotFound)
  }

  test("A PathValidator should capture a variable") {
    val stuff = GET / pv"hello"
    val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

    val f = runWith(stuff) { str: String =>
      Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag(str))))
    }
    checkETag(f(req), "hello")
  }

  test("A PathValidator should work directly") {
    val stuff = GET / "hello"
    val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))

    val f = runWith(stuff) { () =>
      Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag("foo"))))
    }

    checkETag(f(req), "foo")
  }

  test("A PathValidator should capture end with nothing") {
    val stuff = GET / "hello" / *
    val req = Request[IO](uri = Uri.fromString("/hello").getOrElse(sys.error("Failed.")))
    val f = runWith(stuff) { path: List[String] =>
      Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag(if (path.isEmpty) "go" else "nogo"))))
    }

    checkETag(f(req), "go")
  }

  test("A PathValidator should capture remaining") {
    val stuff = GET / "hello" / *
    val req =
      Request[IO](uri = Uri.fromString("/hello/world/foo").getOrElse(sys.error("Failed.")))
    val f = runWith(stuff) { path: List[String] =>
      Ok("Cool.").map(_.putHeaders(ETag(ETag.EntityTag(path.mkString))))
    }

    checkETag(f(req), "worldfoo")
  }

  test("A query validators should get a query string") {
    val path = GET / "hello" +? param[Int]("jimbo")
    val req = Request[IO](uri = Uri.fromString("/hello?jimbo=32").getOrElse(sys.error("Failed.")))

    val route = runWith(path) { i: Int =>
      Ok("stuff").map(_.putHeaders(ETag(ETag.EntityTag((i + 1).toString))))
    }

    checkETag(route(req), "33")
  }

  test("A query validators should accept compound or sequential query rules") {
    val path = GET / "hello"

    val req = Request[IO](uri = uri"/hello?foo=bar&baz=1")

    val route1 = runWith(path +? param[String]("foo") & param[Int]("baz")) { (_: String, _: Int) =>
      Ok("")
    }

    assertEquals(route1(req).value.unsafeRunSync().getOrElse(Response.notFound).status, Status.Ok)

    val route2 = runWith(path +? (param[String]("foo") and param[Int]("baz"))) {
      (_: String, _: Int) =>
        Ok("")
    }

    assertEquals(route2(req).value.unsafeRunSync().getOrElse(Response.notFound).status, Status.Ok)
  }

  test("A query validators should map simple query rules into a complex type") {
    case class Foo(i: Int, f: Double, s: String)
    val paramFoo = (param[Int]("i") & param[Double]("f") & param[String]("s")).map(Foo.apply _)

    val path = GET / "hello" +? paramFoo
    val req =
      Request[IO](uri = Uri.fromString("/hello?i=32&f=3.2&s=Asdf").getOrElse(sys.error("Failed.")))

    val route = runWith(path)((f: Foo) => Ok(s"stuff $f"))

    val result = route(req).value.unsafeRunSync().getOrElse(Response.notFound)
    assertEquals(result.status, Status.Ok)
    assertEquals(RequestRunner.getBody(result.body), "stuff Foo(32,3.2,Asdf)")
  }

  test("Decoders should decode a body") {
    val reqHeader = H[`Content-Length`].existsAnd(h => h.length < 10)

    val path = POST / "hello" >>> reqHeader

    val req1 = Request[IO](POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
      .withEntity("foo")

    val req2 = Request[IO](POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
      .withEntity("0123456789") // length 10

    val route = runWith(path.decoding(EntityDecoder.text)) { str: String =>
      Ok("stuff").map(_.putHeaders(ETag(ETag.EntityTag(str))))
    }

    checkETag(route(req1), "foo")
    assertEquals(
      route(req2).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status,
      Status.BadRequest
    )
  }

  test("Decoders should allow the infix operator syntax") {
    val path = POST / "hello"

    val req = Request[IO](POST, uri = Uri.fromString("/hello").getOrElse(sys.error("Fail")))
      .withEntity("foo")

    val route = runWith(path ^ EntityDecoder.text) { str: String =>
      Ok("stuff").map(_.putHeaders(ETag(ETag.EntityTag(str))))
    }

    checkETag(route(req), "foo")
  }

  test("Decoders should fail on a header") {
    val path = GET / "hello"

    val req = Request[IO](uri = uri"/hello")
      .putHeaders(headers.`Content-Length`.unsafeFromLong("foo".length))

    val reqHeader = H[`Content-Length`].existsAnd(h => h.length < 2)
    val route1 = runWith(path.validate(reqHeader)) { () =>
      Ok("shouldn't get here.")
    }

    assertEquals(
      route1(req).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status,
      Status.BadRequest
    )

    val reqHeaderR = H[`Content-Length`].existsAndR(_ => Some(Unauthorized("Foo.")))
    val route2 = runWith(path.validate(reqHeaderR)) { () =>
      Ok("shouldn't get here.")
    }

    assertEquals(
      route2(req).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status,
      Status.Unauthorized
    )
  }

  test("Decoders should fail on a query") {
    val path = GET / "hello"

    val req = Request[IO](uri = uri"/hello?foo=bar")
      .putHeaders(`Content-Length`.unsafeFromLong("foo".length))

    val route1 = runWith(path +? param[Int]("foo")) { _: Int =>
      Ok("shouldn't get here.")
    }

    assertEquals(
      route1(req).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status,
      Status.BadRequest
    )

    val route2 =
      runWith(path +? paramR[String]("foo", (_: String) => Some(Unauthorized("foo")))) {
        _: String =>
          Ok("shouldn't get here.")
      }

    assertEquals(
      route2(req).value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .status,
      Status.Unauthorized
    )
  }

  val req = Request[IO](uri = uri"/foo/bar")
  val respMsg = "Result"

  test("Path prepending should work for a PathBuilder") {
    val tail = GET / "bar"
    val all = "foo" /: tail

    assertEquals(
      runWith(all)(respMsg)
        .apply(req)
        .value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .as[String]
        .unsafeRunSync(),
      respMsg
    )
  }

  test("Path prepending should work for a QueryBuilder") {
    val tail = GET / "bar" +? param[String]("str")
    val all = "foo" /: tail

    assertEquals(
      runWith(all) { q: String => respMsg + q }
        .apply(req.withUri(uri"/foo/bar?str=answer"))
        .value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .as[String]
        .unsafeRunSync(),
      respMsg + "answer"
    )
  }

  test("Path prepending should work for a Router") {
    val tail = GET / "bar" >>> RequireETag
    val all = "foo" /: tail

    assertEquals(
      runWith(all)(respMsg)
        .apply(req.withUri(uri"/foo/bar").putHeaders(etag))
        .value
        .unsafeRunSync()
        .getOrElse(Response.notFound)
        .as[String]
        .unsafeRunSync(),
      respMsg
    )
  }
}
