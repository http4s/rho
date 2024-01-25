package org.http4s.rho
package swagger

import cats.effect.IO
import cats.syntax.all._
import cats._
import _root_.io.circe._
import _root_.io.circe.parser._
import _root_.io.circe.generic.auto._
import fs2.{Chunk, Stream}
import munit.FunSuite
import org.http4s.Method._
import org.http4s._
import org.http4s.headers._
import org.http4s.rho.bits.PathAST.{PathAnd, PathCapture}
import org.http4s.rho.bits.QueryParser.Params
import org.http4s.rho.bits._
import org.http4s.rho.io._
import org.http4s.rho.swagger.syntax.io._

import scala.collection.compat.immutable.ArraySeq
import scala.reflect.runtime.universe._
import scala.util.Try

object SwaggerModelsBuilderSuite {
  case class Foo(a: String, b: Int)
  case class Bar(c: Long, d: List[Foo])
  case class FooVal(str: String) extends AnyVal

  implicit def jsonParser[A: Decoder: TypeTag]: StringParser[IO, A] = new StringParser[IO, A]
    with FailureResponseOps[IO] {
    override val typeTag: Option[TypeTag[A]] =
      implicitly[TypeTag[A]].some

    override def parse(s: String)(implicit F: Monad[IO]): ResultResponse[IO, A] =
      decode[A](s) match {
        case Left(t) => badRequest[String](t.getMessage)
        case Right(t) => SuccessResponse(t)
      }
  }

  sealed class SealedEnum
  object SealedEnum {
    case object first extends SealedEnum
    case object second extends SealedEnum

    def fromInt(str: String): SealedEnum = str match {
      case "1" => first
      case "2" => second
    }
  }

  implicit val paramQueryParser: QueryParser[IO, SealedEnum] = {
    (name: String, params: Params, _: Option[SealedEnum]) => params.get(name) match {
      case Some(Seq(value, _*)) =>
        Try(SuccessResponse[IO, SealedEnum](SealedEnum.fromInt(value))).getOrElse(badRequest("error"))
      case _ =>
        badRequest("")
    }
  }
}

class SwaggerModelsBuilderSuite extends FunSuite {
  import SwaggerModelsBuilderSuite._
  import models._

  implicit def defaultCompiler: CompileRoutes[IO, RhoRoute.Tpe[IO]] =
    CompileRoutes.identityCompiler

  trait Renderable
  case class ModelA(name: String, color: Int) extends Renderable
  case class ModelB(name: String, id: Long) extends Renderable
  case class ModelC(name: String, shape: String) extends Renderable
  case class ModelMap(bar: String, baz: Map[String, Int]) extends Renderable

  val sb = new SwaggerModelsBuilder[IO](DefaultSwaggerFormats)(
    DefaultShowType,
    implicitly[WeakTypeTag[IO[_]]]
  )
  val fooPath = GET / "foo"
  val barPath = GET / "bar"
  type OpSeq = Option[Seq[String]]

  test("SwaggerModelsBuilder.collectQueryParams should handle head request") {
    val ra = HEAD / "foobar" |>> ""
    assert(sb.collectPaths(ra)(Swagger()).get("/foobar").flatMap(_.head).isDefined)
  }

  test("SwaggerModelsBuilder.collectQueryParams should handle an action with one query parameter") {
    val ra = fooPath +? param[Int]("id") |>> { (_: Int) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(QueryParameter(`type` = "integer".some, name = "id".some, required = true))
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with one query parameter with description"
  ) {
    val ra = fooPath +? paramD[Int]("id", "int id") |>> { (_: Int) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(
          `type` = "integer".some,
          name = "id".some,
          required = true,
          description = "int id".some
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with one optional query parameter"
  ) {
    val ra = fooPath +? param[Option[String]]("name") |>> { (_: Option[String]) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(QueryParameter(`type` = "string".some, name = "name".some, required = false))
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with one optional seq query parameter"
  ) {
    val ra = fooPath +? param[Option[Seq[String]]]("name") |>> { (_: Option[Seq[String]]) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(
          `type` = None,
          name = "name".some,
          items = Some(AbstractProperty(`type` = "string")),
          defaultValue = None,
          isArray = true,
          required = false
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with " +
      "one optional seq query parameter using a type alias"
  ) {
    val ra = fooPath +? param[OpSeq]("name") |>> { (_: OpSeq) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(
          `type` = None,
          name = "name".some,
          items = Some(AbstractProperty(`type` = "string")),
          defaultValue = None,
          isArray = true,
          required = false
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with one query parameter with default value"
  ) {
    val ra = fooPath +? param[Int]("id", 6) |>> { (_: Int) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(
          `type` = "integer".some,
          name = "id".some,
          defaultValue = "6".some,
          required = false
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with " +
      "one query parameter with default value and description"
  ) {
    val ra = fooPath +? paramD[Int]("id", 6, "id with default") |>> { (_: Int) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(
          `type` = "integer".some,
          name = "id".some,
          defaultValue = "6".some,
          required = false,
          description = "id with default".some
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with two query parameters"
  ) {
    val ra = fooPath +? param[Int]("id") & param[String]("str", "hello") |>> {
      (_: Int, _: String) => ""
    }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(`type` = "integer".some, name = "id".some, required = true),
        QueryParameter(
          `type` = "string".some,
          name = "str".some,
          defaultValue = "hello".some,
          required = false
        )
      )
    )
  }

  test("SwaggerModelsBuilder.collectQueryParams should handle an action with query or-structure") {
    def orStr(str: String) = s"Optional if the following params are satisfied: [$str]".some

    val ra = fooPath +? (param[Int]("id") || param[Int]("id2")) |>> { (_: Int) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(
          `type` = "integer".some,
          name = "id".some,
          description = orStr("id2"),
          required = true
        ),
        QueryParameter(
          `type` = "integer".some,
          name = "id2".some,
          description = orStr("id"),
          required = true
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with " +
      "one query parameter of complex data type"
  ) {
    val ra = fooPath +? param[Foo]("foo") |>> { (_: Foo) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(QueryParameter(`type` = "string".some, name = "foo".some, required = true))
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with " +
      "one optional query parameter of complex data type"
  ) {
    val ra = fooPath +? param[Option[Foo]]("foo") |>> { (_: Option[Foo]) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(QueryParameter(`type` = "string".some, name = "foo".some, required = false))
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with " +
      "one optional query parameter of complex (but AnyVal) data type"
  ) {
    val ra = fooPath +? param[Option[FooVal]]("foo") |>> { (_: Option[FooVal]) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(QueryParameter(`type` = "string".some, name = "foo".some, required = false))
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle and action with " +
      "two query parameters of complex data type"
  ) {
    val ra = fooPath +? param[Foo]("foo") & param[Seq[Bar]]("bar", Nil) |>> {
      (_: Foo, _: Seq[Bar]) => ""
    }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(`type` = "string".some, name = "foo".some, required = true),
        QueryParameter(
          `type` = None,
          name = "bar".some,
          items = Some(AbstractProperty(`type` = "string")),
          defaultValue = "".some,
          isArray = true
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with query parameters of empty data types"
  ) {
    val ra = fooPath +? param[Unit]("unit") |>> { (_: Unit) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(`type` = "string".some, name = "unit".some, required = true)
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with one enum query parameter"
  ) {
    val ra = fooPath +? param[SealedEnum]("param") |>> { (_: SealedEnum) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(`type` = "string".some, name = "param".some, required = true, enums = List("first", "second"))
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with one enum query parameter with allowed values"
  ) {
    val ra = fooPath +? paramE[SealedEnum]("param", List("1", "2")) |>> { (_: SealedEnum) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(`type` = "string".some, name = "param".some, required = true, enums = List("1", "2"))
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectQueryParams should handle an action with one enum query parameter with allowed values and default value"
  ) {
    val ra = fooPath +? paramE[SealedEnum]("param", SealedEnum.first, List("1", "2")) |>> { (_: SealedEnum) => "" }

    assertEquals(
      sb.collectQueryParams(singleLinearRoute(ra)),
      List(
        QueryParameter(`type` = "string".some, name = "param".some, enums = List("1", "2"), defaultValue = Some("first"))
      )
    )
  }

  test("SwaggerModelsBuilder.collectHeaderParams should handle an action with single header rule") {
    val ra = fooPath >>> H[`Content-Length`].exists |>> { () => "" }

    assertEquals(
      sb.collectHeaderParams(singleLinearRoute(ra)),
      List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = true))
    )
  }

  test("SwaggerModelsBuilder.collectHeaderParams should handle an action with two header rules") {
    val ra = fooPath >>> (H[`Content-Length`].exists && H[`Content-Type`].exists) |>> { () => "" }

    assertEquals(
      sb.collectHeaderParams(singleLinearRoute(ra)),
      List(
        HeaderParameter(`type` = "string", name = "Content-Length".some, required = true),
        HeaderParameter(`type` = "string", name = "Content-Type".some, required = true)
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectHeaderParams should handle an action with or-structure header rules"
  ) {
    def orStr(str: String) = s"Optional if the following headers are satisfied: [$str]".some

    val ra = fooPath >>> (H[`Content-Length`].exists || H[`Content-Type`].exists) |>> { () => "" }

    assertEquals(
      sb.collectHeaderParams(singleLinearRoute(ra)),
      List(
        HeaderParameter(
          `type` = "string",
          name = "Content-Length".some,
          description = orStr("Content-Type"),
          required = true
        ),
        HeaderParameter(
          `type` = "string",
          name = "Content-Type".some,
          description = orStr("Content-Length"),
          required = true
        )
      )
    )
  }

  test(
    "SwaggerModelsBuilder.collectHeaderParams should set required = false if there is a default value for the header"
  ) {
    val ra =
      fooPath >>> H[`Content-Length`].captureOrElse(`Content-Length`.unsafeFromLong(20)) |>> {
        (_: `Content-Length`) => ""
      }

    assertEquals(
      sb.collectHeaderParams(singleLinearRoute(ra)),
      List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = false))
    )
  }

  test(
    "SwaggerModelsBuilder.collectHeaderParams should set required = false if the header is optional"
  ) {
    val ra = fooPath >>> H[`Content-Length`].captureOptionally |>> {
      (_: Option[`Content-Length`]) => ""
    }

    assertEquals(
      sb.collectHeaderParams(singleLinearRoute(ra)),
      List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = false))
    )
  }

  test(
    "SwaggerModelsBuilder.collectHeaderParams should set required = false " +
      "by default if there is a missingHeaderResult for the header"
  ) {
    val ra = fooPath >>> H[`Content-Length`].captureMapR(Option(Ok("5")))(Right(_)) |>> {
      (_: `Content-Length`) => ""
    }

    assertEquals(
      sb.collectHeaderParams(singleLinearRoute(ra)),
      List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = false))
    )
  }

  test("SwaggerModelsBuilder.collectBodyParams should handle a required body parameter") {
    val dec = new EntityDecoder[IO, Foo] {
      override def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, Foo] = ???
      override def consumes: Set[MediaRange] = Set.empty

    }
    val ra = fooPath.decoding(dec) |>> { _: Foo => "" }

    assert(sb.collectBodyParams(singleLinearRoute(ra)).exists(_.required))
  }

  test("SwaggerModelsBuilder.collectBodyParams should handle an optional body parameter") {
    val dec = new EntityDecoder[IO, Option[Foo]] {
      override def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, Option[Foo]] = ???
      override def consumes: Set[MediaRange] = Set.empty
    }
    val ra = fooPath.decoding(dec) |>> { _: Option[Foo] => "" }

    assert(sb.collectBodyParams(singleLinearRoute(ra)).exists(_.required == false))
  }

  test("SwaggerModelsBuilder.mkOperation should get a route description") {
    val ra = "foo" ** GET / "bar" |>> { () => "" }

    assertEquals(sb.mkOperation(singleLinearRoute(ra)).summary, "foo".some)
  }

  test("SwaggerModelsBuilder.mkOperation should get explicit route tag") {
    val ra = "tag" @@ GET / "bar" |>> { () => "" }
    assertEquals(sb.mkOperation(singleLinearRoute(ra)).tags, List("tag"))
  }

  test("SwaggerModelsBuilder.mkOperation should get explicit route tags") {
    val ra = List("tag1", "tag2") @@ GET / "bar" |>> { () => "" }

    assertEquals(sb.mkOperation(singleLinearRoute(ra)).tags, List("tag1", "tag2"))
  }

  test("SwaggerModelsBuilder.mkOperation should default to first segment for tags") {
    val ra = GET / "foo" / "bar" |>> { () => "" }

    assertEquals(sb.mkOperation(singleLinearRoute(ra)).tags, List("foo"))
  }

  test(
    "SwaggerModelsBuilder.mkOperation should default to / as a tag for routes without segments"
  ) {
    val ra = GET |>> { () => "" }
    assertEquals(sb.mkOperation(singleLinearRoute(ra)).tags, List("/"))
  }

  test("SwaggerModelsBuilder.mkOperation should mix and match route descriptions and tags") {
    val ra1 = "foo" ** List("tag1", "tag2") @@ GET / "bar" |>> { () => "" }
    val ra2 = List("tag1", "tag2") @@ ("foo" ** GET / "bar") |>> { () => "" }
    val ra3 = "foo" ** "tag" @@ GET / "bar" |>> { () => "" }
    val ra4 = "tag" @@ ("foo" ** GET / "bar") |>> { () => "" }

    assertEquals(sb.mkOperation(singleLinearRoute(ra1)), sb.mkOperation(singleLinearRoute(ra2)))
    assertEquals(sb.mkOperation(singleLinearRoute(ra3)), sb.mkOperation(singleLinearRoute(ra4)))
  }

  test("SwaggerModelsBuilder.mkOperation should preserve explicit tags after prepending segments") {
    val inner = List("tag1", "tag2") @@ GET / "bar" |>> { () => "" }
    val ra = "foo" /: inner

    assertEquals(sb.mkOperation(singleLinearRoute(ra)).tags, List("tag1", "tag2"))
  }

  test("SwaggerModelsBuilder.mkOperation should produce unique operation ids") {
    val routes = Seq(
      "foo1" ** GET / "bar" |>> { () => "" },
      "foo2" ** GET / "bar" / pathVar[String]("name") |>> { _: String => "" },
      "foo3" ** GET / "bar" / pathVar[String]("name1") / pathVar[String]("name2") |>> {
        (_: String, _: String) => ""
      }
    )

    val operationIds = routes
      .foldLeft(Swagger())((s, r) => sb.mkSwagger(r)(s))
      .paths
      .values
      .toList
      .flatMap(_.get)
      .flatMap(_.operationId)

    assertEquals(operationIds, List("getBar", "getBar-name", "getBar-name1-name2"))
  }

  test("SwaggerModelsBuilder.mkOperation should create Security Scope") {
    val x: Map[String, List[String]] = Map("oauth" -> List("admin"))

    val ra = x ^^ "foo1" ** GET / "bar" |>> { () => "" }

    assertEquals(sb.mkOperation(singleLinearRoute(ra)).security, List(x))
  }

  test("SwaggerModelsBuilder.mkOperation should create Security Scopes") {
    val x: Map[String, List[String]] =
      Map("oauth" -> List("admin", "read"), "apiKey" -> List("read"))

    val ra = x ^^ "foo1" ** GET / "bar" |>> { () => "" }

    assertEquals(sb.mkOperation(singleLinearRoute(ra)).security, List(x))
  }

  test("SwaggerModelsBuilder.mkOperation should handle collection params in body") {
    val dec = new EntityDecoder[IO, List[String]] {
      override def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, List[String]] = ???
      override def consumes: Set[MediaRange] = Set.empty
    }

    val ra = POST / "foo" ^ dec |>> { (_: List[String]) => "" }

    assertEquals(
      sb.mkOperation(singleLinearRoute(ra)).parameters,
      List(
        BodyParameter(
          schema = ArrayModel(
            id = "scala.collection.immutable.List«java.lang.String»",
            id2 = "scala.collection.immutable.List«java.lang.String»",
            `type` = "array".some,
            items = RefProperty(ref = "String", title = "String".some).some
          ).some,
          name = "body".some,
          description = "List«String»".some,
          required = true
        )
      )
    )
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple path - GET") {
    val ra = GET / "foo" |>> { () => "" }
    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == ("/foo", Path(get = sb.mkOperation(singleLinearRoute(ra)).some))))
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple path - PUT") {
    val ra = PUT / "foo" |>> { () => "" }
    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == ("/foo", Path(put = sb.mkOperation(singleLinearRoute(ra)).some))))
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple path - POST") {
    val ra = POST / "foo" |>> { () => "" }
    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == "/foo" -> Path(post = sb.mkOperation(singleLinearRoute(ra)).some)))
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple path - PATCH") {
    val ra = PATCH / "foo" |>> { () => "" }
    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == "/foo" -> Path(patch = sb.mkOperation(singleLinearRoute(ra)).some)))
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple path - OPTIONS") {
    val ra = OPTIONS / "foo" |>> { () => "" }
    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == "/foo" -> Path(options = sb.mkOperation(singleLinearRoute(ra)).some)))
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple and-path") {
    val ra = GET / "foo" / "bar" |>> { () => "" }
    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == "/foo/bar" -> Path(get = sb.mkOperation(singleLinearRoute(ra)).some)))
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple or-path") {
    val ra = GET / ("foo" || "bar") |>> { () => "" }

    val rFoo = GET / "foo" |>> { () => "" }
    val rBar = GET / "bar" |>> { () => "" }

    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == "/foo" -> Path(get = sb.mkOperation(singleLinearRoute(rFoo)).some)))
    assert(paths.exists(_ == "/bar" -> Path(get = sb.mkOperation(singleLinearRoute(rBar)).some)))
  }

  test("SwaggerModelsBuilder.collectPaths should find a capture or-path") {
    val ra = GET / (pathVar[Int]("foo") || pathVar[Int]("bar")) |>> { (_: Int) => "" }

    val rFoo = GET / pathVar[Int]("foo") |>> { (_: Int) => "" }
    val rBar = GET / pathVar[Int]("bar") |>> { (_: Int) => "" }

    val paths = sb.collectPaths(ra)(Swagger())

    assert(paths.exists(_ == "/{foo}" -> Path(get = sb.mkOperation(singleLinearRoute(rFoo)).some)))
    assert(paths.exists(_ == "/{bar}" -> Path(get = sb.mkOperation(singleLinearRoute(rBar)).some)))
  }

  test("SwaggerModelsBuilder.collectPaths should find a simple path with a capture") {
    val ra = GET / "foo" / pathVar[Int]("number") |>> { (_: Int) => "" }

    assert(
      sb.collectPaths(ra)(Swagger())
        .exists(
          _ == "/foo/{number}" -> Path(get = sb.mkOperation(singleLinearRoute(ra)).some)
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectPaths should find a simple path with a capture with description"
  ) {
    val ra = GET / pathVar[Int]("number", "int pathVar") |>> { (_: Int) => "" }

    assert(
      sb.collectPaths(ra)(Swagger())
        .exists(
          _ == "/{number}" -> Path(get = sb.mkOperation(singleLinearRoute(ra)).some)
        )
    )

    ra.path match {
      case PathAnd(_, p) =>
        p match {
          case PathCapture(_, description, _, _) =>
            assertEquals(description, "int pathVar".some)

          case _ =>
            fail("Unexpected SwaggerModelsBuilder.collectPaths result found")
        }

      case _ =>
        fail("Unexpected SwaggerModelsBuilder.collectPaths result found")
    }
  }

  test("SwaggerModelsBuilder.collectPaths should maintain order of paths") {
    def route(prefix: String) =
      GET / prefix / pathVar[Int]("number", "int pathVar") |>> { (_: Int) => "" }
    def result(prefix: String) =
      s"/${prefix}/{number}" -> Path(get = sb.mkOperation(singleLinearRoute(route(prefix))).some)

    def build(s: Swagger, r2: RhoRoute[IO, _]) =
      Swagger(paths = sb.collectPaths(r2)(s))

    val prefixRange = 0 to 10
    val routes = prefixRange.map(i => s"p$i").map(route).toList
    val compiledRoutes = routes.foldLeft(Swagger())(build).paths.toList
    val results = prefixRange.map(i => s"p$i").map(result).toList

    assertEquals(compiledRoutes, results)
  }

  test("SwaggerModelsBuilder.collectPaths should generate precise consumed media types") {
    val dec = new EntityDecoder[IO, List[String]] {
      def consumes: Set[MediaRange] = Set(MediaType.application.json)
      def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, List[String]] = ???
    }
    val ra = GET / "foo" ^ dec |>> { _: List[String] => "" }
    val op = sb.mkOperation(singleLinearRoute(ra))

    assertEquals(op.consumes, List("application/json"))
  }

  private val prefix = "org.http4s.rho.swagger.SwaggerModelsBuilderSuite."
  private val modelAFullName = prefix + "ModelA"
  private val modelBFullName = prefix + "ModelB"
  private val modelCFullName = prefix + "ModelC"

  test("SwaggerModelsBuilder.collectDefinitions should get available models") {
    val ra = "testing models" ** GET / "models" |>> { () =>
      val a = 0
      a match {
        case 0 => Ok(ModelA("modela", 1))
        case 1 => NotFound(ModelB("modelb", 2))
        case 2 => PreconditionFailed(ModelC("modelc", "round"))
      }
    }

    val paths = sb.collectDefinitions(ra)(Swagger())

    assert(
      paths.exists(
        _ == "ModelA" ->
          ModelImpl(
            id = modelAFullName,
            id2 = "ModelA",
            description = "ModelA".some,
            `type` = "object".some,
            properties = Map(
              "name" -> AbstractProperty("string", None, true),
              "color" -> AbstractProperty("integer", None, true, format = "int32".some)
            )
          )
      )
    )

    assert(
      paths.exists(
        _ == "ModelB" ->
          ModelImpl(
            id = modelBFullName,
            id2 = "ModelB",
            description = "ModelB".some,
            `type` = "object".some,
            properties = Map(
              "name" -> AbstractProperty("string", None, true),
              "id" -> AbstractProperty("integer", None, true, format = "int64".some)
            )
          )
      )
    )

    assert(
      paths.exists(
        _ == "ModelC" ->
          ModelImpl(
            id = modelCFullName,
            id2 = "ModelC",
            description = "ModelC".some,
            `type` = "object".some,
            properties = Map(
              "name" -> AbstractProperty("string", None, true),
              "shape" -> AbstractProperty("string", None, true)
            )
          )
      )
    )
  }

  test("SwaggerModelsBuilder.collectDefinitions should handle models with parameters") {
    val ra = "testing models" ** GET / "models" |>> { () =>
      Ok((1, ModelA("modela", 1)))
    }

    assert(
      sb.collectDefinitions(ra)(Swagger())
        .exists(
          _ == "Tuple2«Int,ModelA»" ->
            ModelImpl(
              id = s"scala.Tuple2«scala.Int,$modelAFullName»",
              id2 = "Tuple2«Int,ModelA»",
              description = "Tuple2«Int,ModelA»".some,
              `type` = "object".some,
              properties = Map(
                "_1" -> AbstractProperty("integer", None, true, format = "int32".some),
                "_2" -> RefProperty(ref = "ModelA", required = true)
              )
            )
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectDefinitions should collect response of case class containing a map of primitive types"
  ) {
    val ra = GET / "test" |>> { () => Ok(ModelMap("asdf", Map("foo" -> 1))) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = RefProperty("ModelMap", false, None, None, None).some
          )
        )
    )

    assert(
      sb.collectDefinitions(ra)(Swagger())
        .exists(
          _ == "ModelMap" ->
            ModelImpl(
              id = prefix + "ModelMap",
              id2 = "ModelMap",
              description = "ModelMap".some,
              `type` = "object".some,
              properties = Map(
                "bar" -> AbstractProperty(`type` = "string", required = true),
                "baz" -> MapProperty(
                  required = true,
                  additionalProperties = AbstractProperty("integer", format = "int32".some)
                )
              )
            )
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectResponses should collect an empty response with response code forbidding entity"
  ) {
    val ra = GET / "test" |>> { () => NoContent.apply }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "204" -> Response(description = "No Content", schema = None)
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectResponses should collect an empty response with response code allowing entity"
  ) {
    val ra = GET / "test" |>> { () => Ok(()) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(description = "OK", schema = None)
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of primitive types") {
    val ra = GET / "test" |>> { () => Ok("") }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = AbstractProperty(`type` = "string").some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response with file type") {
    val ra = GET / "test" |>> { () => Ok(SwaggerFileResponse(CsvFile())) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = AbstractProperty(`type` = "file").some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of user-defined types") {
    val ra = GET / "test" |>> { () => Ok(ModelA("", 0)) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(description = "OK", schema = RefProperty(ref = "ModelA").some)
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectResponses should collect response of collection of primitive types"
  ) {
    val ra = GET / "test" |>> { () => Ok(List("")) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = ArrayProperty(items = AbstractProperty(`type` = "string")).some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of collection of unit") {
    val ra = GET / "test" |>> { () => Ok(List(())) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = ArrayProperty(items = RefProperty(ref = "Unit")).some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of map of primitive types") {
    val ra = GET / "test" |>> { () => Ok(Map("foo" -> "bar")) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = MapProperty(additionalProperties = AbstractProperty(`type` = "string")).some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of map of unit") {
    val ra = GET / "test" |>> { () => Ok(Map(("foo", ()))) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = MapProperty(additionalProperties = RefProperty(ref = "Unit")).some
          )
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectResponses should collect response of collection of user-defined types"
  ) {
    val ra = GET / "test" |>> { () => Ok(List(ModelA("", 0))) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = ArrayProperty(items = RefProperty(ref = "ModelA")).some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of tuples") {
    val ra = GET / "test" |>> { () => Ok((0, ModelA("", 0))) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = RefProperty(ref = "Tuple2«Int,ModelA»").some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of a collection of tuples") {
    val ra = GET / "test" |>> { () => Ok(List((0, ModelA("", 0)))) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of a Stream of primitives") {
    val ra1 = GET / "test" |>> { () => Ok(Stream.eval(IO.pure(""))) }
    val ra2 = GET / "test" |>> { () => Ok(Stream.emit("").covary[IO]) }

    assert(
      sb.collectResponses(singleLinearRoute(ra1))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = AbstractProperty(`type` = "string").some
          )
        )
    )

    assert(
      sb.collectResponses(singleLinearRoute(ra2))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = AbstractProperty(`type` = "string").some
          )
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectResponses should collect response of a Stream of non-primitives"
  ) {
    val ra1 = GET / "test" |>> { () => Ok(Stream.eval(IO.pure(List((0, ModelA("", 0)))))) }
    val ra2 = GET / "test" |>> { () => Ok(Stream.emit(List((0, ModelA("", 0)))).covary[IO]) }

    assert(
      sb.collectResponses(singleLinearRoute(ra1))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some
          )
        )
    )

    assert(
      sb.collectResponses(singleLinearRoute(ra2))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect response of an IO of a primitive") {
    val ra = GET / "test" |>> { () => Ok("") }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = AbstractProperty(`type` = "string").some
          )
        )
    )
  }

  test(
    "SwaggerModelsBuilder.collectResponses should collect response of an IO of a non-primitive"
  ) {
    val ra = GET / "test" |>> { () => Ok(List((0, ModelA("", 0)))) }

    assert(
      sb.collectResponses(singleLinearRoute(ra))
        .exists(
          _ == "200" -> Response(
            description = "OK",
            schema = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some
          )
        )
    )
  }

  test("SwaggerModelsBuilder.collectResponses should collect multiple responses") {
    val ra = GET / "test" / pathVar[Int] |>> { (i: Int) =>
      i match {
        case 0 => Ok(List((0, ModelA("A", 0))))
        case _ => Unauthorized("Unauthorized")
      }
    }

    val paths = sb.collectResponses(singleLinearRoute(ra))

    assert(
      paths.exists(
        _ == "200" -> Response(
          description = "OK",
          schema = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some
        )
      )
    )

    assert(
      paths.exists(
        _ == "401" -> Response(
          description = "Unauthorized",
          schema = AbstractProperty(`type` = "string").some
        )
      )
    )
  }

  def singleLinearRoute(rr: RhoRoute[IO, _]): sb.LinearRoute =
    sb.linearizeRoute(rr) match {
      case Seq(lr) => lr
      case other =>
        throw new Exception(
          s"Expected $rr\n to linearize to exactly one LinearRoute. Instead got:\n${other.mkString("\n")}"
        )
    }

  implicit def renderableEncoder[F[_], T <: Renderable]: EntityEncoder[F, T] =
    EntityEncoder
      .stringEncoder[F](Charset.`UTF-8`)
      .contramap { _: T => "" }
      .withContentType(`Content-Type`(MediaType.application.json, Charset.`UTF-8`))

  implicit def tuple2Encoder[F[_], T <: Renderable]: EntityEncoder[F, (Int, T)] =
    EntityEncoder
      .stringEncoder[F](Charset.`UTF-8`)
      .contramap { _: (Int, T) => "" }
      .withContentType(`Content-Type`(MediaType.application.json, Charset.`UTF-8`))

  implicit def listEntityEncoder[F[_], A]: EntityEncoder[F, List[A]] =
    EntityEncoder.simple[F, List[A]]()(_ => Chunk.array("A".getBytes))

  implicit def mapEntityEncoder[F[_], A, B]: EntityEncoder[F, Map[A, B]] =
    EntityEncoder.simple[F, Map[A, B]]()(_ => Chunk.array("A".getBytes))

  case class CsvFile()

  object CsvFile {
    implicit def entityEncoderCsvFile: EntityEncoder[IO, CsvFile] =
      EntityEncoder.encodeBy[IO, CsvFile](
        `Content-Type`(MediaType.text.csv, Some(Charset.`UTF-8`))
      ) { _: CsvFile =>
        val bv = "file content".getBytes(Charset.`UTF-8`.nioCharset)
        org.http4s.Entity(Stream.emits(ArraySeq.unsafeWrapArray(bv)), Some(bv.length))
      }
  }
}
