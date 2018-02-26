package org.http4s.rho
package swagger

import cats.effect.IO
import cats.syntax.all._
import cats.{Applicative, Monad}
import fs2.{Chunk, Stream}
import org.http4s.Method._
import org.http4s._
import org.http4s.headers._
import org.http4s.rho.bits.PathAST.{PathAnd, PathCapture}
import org.http4s.rho.bits._
import org.http4s.rho.io._
import org.http4s.rho.swagger.syntax.io._
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

import scala.language.existentials
import scala.reflect._
import scala.reflect.runtime.universe._

object SwaggerModelsBuilderSpec {
  case class Foo(a: String, b: Int)
  case class Bar(c: Long, d: List[Foo])
  case class FooVal(str: String) extends AnyVal

  import org.json4s._
  import org.json4s.jackson.JsonMethods

  private implicit val format: DefaultFormats =
    DefaultFormats

  implicit def jsonParser[A : TypeTag : ClassTag]: StringParser[IO, A] = new StringParser[IO, A] with FailureResponseOps[IO] {
    override val typeTag: Option[TypeTag[A]] =
      implicitly[TypeTag[A]].some

    override def parse(s: String)(implicit F: Monad[IO]): ResultResponse[IO, A] = {
      Either.catchNonFatal(JsonMethods.parse(s).extract[A]) match {
        case Left(t) => badRequest[String](t.getMessage)
        case Right(t) => SuccessResponse(t)
      }
    }
  }
}

class SwaggerModelsBuilderSpec extends Specification {
  import SwaggerModelsBuilderSpec._
  import models._

  implicit def defaultCompiler: CompileService[IO, RhoRoute.Tpe[IO]] =
    CompileService.identityCompiler

  sealed abstract class Renderable
  case class ModelA(name: String, color: Int) extends Renderable
  case class ModelB(name: String, id: Long) extends Renderable
  case class ModelC(name: String, shape: String) extends Renderable
  case class ModelMap(bar: String, baz: Map[String, Int]) extends Renderable

  val sb = new SwaggerModelsBuilder(DefaultSwaggerFormats)
  val fooPath = GET / "foo"
  val barPath = GET / "bar"

  "SwaggerModelsBuilder.collectQueryParams" should {

    "handle head request" in {
      val ra = HEAD / "foobar" |>> { "" }
      sb.collectPaths[IO](ra)(Swagger()).get("/foobar").flatMap(_.head) must beSome[Operation]
    }

    "handle an action with one query parameter" in {
      val ra = fooPath +? param[Int]("id") |>> { (i: Int) => "" }

      sb.collectQueryParams[IO](ra) must_==
      List(QueryParameter(`type` = "integer".some, name = "id".some, required = true))
    }

    "handle an action with one query parameter with description" in {
      val ra = fooPath +? paramD[Int]("id", "int id") |>> { (i: Int) => "" }

      sb.collectQueryParams[IO](ra) must_==
        List(QueryParameter(`type` = "integer".some, name = "id".some, required = true, description = "int id".some))
    }

    "handle an action with one optional query parameter" in {
      val ra = fooPath +? param[Option[String]]("name") |>> { (s: Option[String]) => "" }

      sb.collectQueryParams[IO](ra) must_==
      List(QueryParameter(`type` = "string".some, name = "name".some, required = false))
    }

    "handle an action with one query parameter with default value" in {
      val ra = fooPath +? param[Int]("id", 6) |>> { (i: Int) => "" }

      sb.collectQueryParams[IO](ra) must_==
      List(QueryParameter(`type` = "integer".some, name = "id".some, defaultValue = "6".some, required = false))
    }

    "handle an action with one query parameter with default value and description" in {
      val ra = fooPath +? paramD[Int]("id", 6, "id with default") |>> { (i: Int) => "" }

      sb.collectQueryParams[IO](ra) must_==
        List(QueryParameter(`type` = "integer".some, name = "id".some, defaultValue = "6".some, required = false, description = "id with default".some))
    }

    "handle an action with two query parameters" in {
      val ra = fooPath +? param[Int]("id") & param[String]("str", "hello") |>> { (i: Int, s: String) => "" }

      sb.collectQueryParams[IO](ra) must_==
      List(
        QueryParameter(`type` = "integer".some, name = "id".some,  required = true),
        QueryParameter(`type` = "string".some,  name = "str".some, defaultValue = "hello".some, required = false))
    }

    "handle an action with query or-structure" in {
      def orStr(str: String) = s"Optional if the following params are satisfied: [$str]".some

      val ra = fooPath +? (param[Int]("id") || param[Int]("id2")) |>> { (i: Int) => "" }

      sb.collectQueryParams[IO](ra) must_==
      List(
        QueryParameter(`type` = "integer".some, name = "id".some,  description = orStr("id2"), required = true),
        QueryParameter(`type` = "integer".some, name = "id2".some, description = orStr("id"),  required = true))
    }

    "handle an action with one query parameter of complex data type" in {
       val ra = fooPath +? param[Foo]("foo") |>> { (_: Foo) => "" }

       sb.collectQueryParams[IO](ra) must_==
       List(QueryParameter(`type` = "string".some, name = "foo".some, required = true))
    }

    "handle an action with one optional query parameter of complex data type" in {
       val ra = fooPath +? param[Option[Foo]]("foo") |>> { (_: Option[Foo]) => "" }

       sb.collectQueryParams[IO](ra) must_==
       List(QueryParameter(`type` = "string".some, name = "foo".some, required = false))
    }

    "handle an action with one optional query parameter of complex (but AnyVal) data type" in {
       val ra = fooPath +? param[Option[FooVal]]("foo") |>> { (_: Option[FooVal]) => "" }

       sb.collectQueryParams[IO](ra) must_==
       List(QueryParameter(`type` = "string".some, name = "foo".some, required = false))
    }

    "handle and action with two query parameters of complex data type" in {
      val ra = fooPath +? param[Foo]("foo") & param[Seq[Bar]]("bar", Nil) |>> { (_: Foo, _: Seq[Bar]) => "" }

      sb.collectQueryParams[IO](ra) must_==
      List(
        QueryParameter(`type` = "string".some, name = "foo".some, required = true),
        QueryParameter(`type` = None, name = "bar".some, items = Some(AbstractProperty(`type` = "string")), defaultValue = "".some, isArray = true)
      )
    }
  }

  "SwaggerModelsBuilder.collectHeaderParams" should {

    "handle an action with single header rule" in {
      val ra = fooPath >>> exists(`Content-Length`) |>> { () => "" }

      sb.collectHeaderParams[IO](ra) must_==
      List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = true))
    }

    "handle an action with two header rules" in {
      val ra = fooPath >>> (exists(`Content-Length`) && exists(`Content-MD5`)) |>> { () => "" }

      sb.collectHeaderParams[IO](ra) must_==
      List(
        HeaderParameter(`type` = "string", name = "Content-Length".some, required = true),
        HeaderParameter(`type` = "string", name = "Content-MD5".some,    required = true))
    }

    "handle an action with or-structure header rules" in {
      def orStr(str: String) = s"Optional if the following headers are satisfied: [$str]".some

      val ra = fooPath >>> (exists(`Content-Length`) || exists(`Content-MD5`)) |>> { () => "" }

      sb.collectHeaderParams[IO](ra) must_==
      List(
        HeaderParameter(`type` = "string", name = "Content-Length".some, description = orStr("Content-MD5"), required = true),
        HeaderParameter(`type` = "string", name = "Content-MD5".some,    description = orStr("Content-Length"), required = true))
    }

    "set required = false if there is a default value for the header" in {
      val ra = fooPath >>> captureMapR(`Content-Length`, Option(Ok("5")))(Right(_)) |>> { (_: `Content-Length`) => "" }

      sb.collectHeaderParams[IO](ra) must_==
        List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = false))
    }

  }

  "SwaggerModelsBuilder.mkOperation" should {

    "Get a route description" in {
      val ra = "foo" ** GET / "bar" |>> { () => "" }

      sb.mkOperation("/foo", ra).summary must_== "foo".some
    }

    "Produce unique operation ids" in {
      val routes = Seq(
        "foo1" ** GET / "bar" |>> { () => "" },
        "foo2" ** GET / "bar" / pathVar[String]("name") |>> { name: String => "" },
        "foo3" ** GET / "bar" / pathVar[String]("name1") / pathVar[String]("name2") |>> { (name1: String, name2: String) => "" }
      )

      val operationIds = routes.foldLeft(Swagger())((s, r) => sb.mkSwagger(Info("", ""), r)(s)).paths.values.toList.flatMap(_.get).flatMap(_.operationId)

      operationIds ==== List("getBar", "getBar-name", "getBar-name1-name2")
      }

    "Create Security Scope" in {
      val x:Map[String, List[String]]  = Map("oauth" -> List("admin"))

      val ra = x ^^ "foo1" ** GET / "bar" |>> { () => "" }

      sb.mkOperation("/foo", ra).security must_== List(x)
    }

    "Create Security Scopes" in {
      val x:Map[String, List[String]]  = Map("oauth" -> List("admin", "read"),"apiKey" -> List("read"))

      val ra = x ^^ "foo1" ** GET / "bar" |>> { () => "" }

      sb.mkOperation("/foo", ra).security must_== List(x)
    }

    "Handle collection params in body" in {
      val dec = new EntityDecoder[IO, List[String]] {
        override def decode(msg: Message[IO], strict: Boolean): DecodeResult[IO, List[String]] = ???
        override def consumes: Set[MediaRange] = Set.empty
      }

      val ra = POST / "foo" ^ dec |>> { (l: List[String]) => "" }

      sb.mkOperation("/foo", ra).parameters must_== List(
        BodyParameter(
          schema = ArrayModel(
            id = "scala.collection.immutable.List«java.lang.String»",
            id2 = "scala.collection.immutable.List«java.lang.String»",
            `type` = "array".some,
            items = RefProperty(ref = "String", title = "String".some).some).some,
          name = "body".some,
          description = "List«String»".some
        )
      )
    }
  }

  "SwaggerModelsBuilder.collectPaths" should {

    "find a simple path - GET" in {
      val ra = GET / "foo" |>> { () => "" }
      val paths = sb.collectPaths[IO](ra)(Swagger())

      paths must havePair("/foo" -> Path(get = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - PUT" in {
      val ra = PUT / "foo" |>> { () => "" }
      val paths = sb.collectPaths[IO](ra)(Swagger())

      paths must havePair("/foo" -> Path(put = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - POST" in {
      val ra = POST / "foo" |>> { () => "" }
      val paths = sb.collectPaths[IO](ra)(Swagger())

      paths must havePair("/foo" -> Path(post = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - PATCH" in {
      val ra = PATCH / "foo" |>> { () => "" }
      val paths = sb.collectPaths[IO](ra)(Swagger())

      paths must havePair("/foo" -> Path(patch = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - OPTIONS" in {
      val ra = OPTIONS / "foo" |>> { () => "" }
      val paths = sb.collectPaths[IO](ra)(Swagger())

      paths must havePair("/foo" -> Path(options = sb.mkOperation("/foo", ra).some))
    }

    "find a simple and-path" in {
      val ra = GET / "foo" / "bar" |>> { () => "" }
      val paths = sb.collectPaths[IO](ra)(Swagger())

      paths must havePair("/foo/bar" -> Path(get = sb.mkOperation("/foo/bar", ra).some))
    }

    "find a simple or-path" in {
      val ra = GET / ("foo" || "bar") |>> { () => "" }

      sb.collectPaths[IO](ra)(Swagger()) must havePairs(
        "/foo" -> Path(get = sb.mkOperation("/foo", ra).some),
        "/bar" -> Path(get = sb.mkOperation("/bar", ra).some))
    }

    "find a capture or-path" in {
      val ra = GET / (pathVar[Int]("foo") || pathVar[Int]("bar")) |>> { (i: Int) => "" }

      sb.collectPaths[IO](ra)(Swagger()) must havePairs(
        "/{foo}" -> Path(get = sb.mkOperation("{foo}", ra).some),
        "/{bar}" -> Path(get = sb.mkOperation("{bar}", ra).some))
    }

    "find a simple path with a capture" in {
      val ra = GET / "foo" / pathVar[Int]("number") |>> { (i: Int) => "" }

      sb.collectPaths[IO](ra)(Swagger()) must havePair(
        "/foo/{number}" -> Path(get = sb.mkOperation("foo/{number}", ra).some))
    }

    "find a simple path with a capture with description" in {
      val ra = GET / pathVar[Int]("number", "int pathVar") |>> { (i: Int) => "" }

      sb.collectPaths[IO](ra)(Swagger()) must havePair(
      "/{number}" -> Path(get = sb.mkOperation("/{number}", ra).some))

      ra.path match {
        case PathAnd(_, p) =>
          p must beAnInstanceOf[PathCapture[IO]]
          p.asInstanceOf[PathCapture[IO]].description must_== "int pathVar".some

        case _ => false must_== true
      }
    }
  }

  "SwaggerModelsBuilder.collectDefinitions" should {

    val prefix = "org.http4s.rho.swagger.SwaggerModelsBuilderSpec."
    val modelAFullName = prefix + "ModelA"
    val modelBFullName = prefix + "ModelB"
    val modelCFullName = prefix + "ModelC"

    "get available models" in {

      val ra = "testing models" ** GET / "models" |>> { () =>
        val a = 0
        a match {
          case 0 => Ok(ModelA("modela", 1))
          case 1 => NotFound(ModelB("modelb", 2))
          case 2 => PreconditionFailed(ModelC("modelc", "round"))
        }
      }

      sb.collectDefinitions[IO](ra)(Swagger()) must havePairs(

        "ModelA" ->
          ModelImpl(
            id          = modelAFullName,
            id2         = "ModelA",
            description = "ModelA".some,
            `type`      = "object".some,
            properties  = Map(
              "name"  -> AbstractProperty("string", None, true),
              "color" -> AbstractProperty("integer", None, true, format = "int32".some))),

        "ModelB" ->
          ModelImpl(
            id          = modelBFullName,
            id2         = "ModelB",
            description = "ModelB".some,
            `type`      = "object".some,
            properties  = Map(
              "name" -> AbstractProperty("string", None, true),
              "id"   -> AbstractProperty("integer", None, true, format = "int64".some))),

        "ModelC" ->
          ModelImpl(
            id          = modelCFullName,
            id2         = "ModelC",
            description = "ModelC".some,
            `type`      = "object".some,
            properties  = Map(
              "name"  -> AbstractProperty("string", None, true),
              "shape" -> AbstractProperty("string", None, true)))
      )
    }

    "handle models with parameters" in {

      val ra = "testing models" ** GET / "models" |>> { () =>
        Ok((1, ModelA("modela", 1)))
      }

      sb.collectDefinitions[IO](ra)(Swagger()) must havePairs(

        "Tuple2«Int,ModelA»" ->
          ModelImpl(
            id          = s"scala.Tuple2«scala.Int,$modelAFullName»",
            id2         = "Tuple2«Int,ModelA»",
            description = "Tuple2«Int,ModelA»".some,
            `type`      = "object".some,
            properties  = Map(
              "_1" -> AbstractProperty("integer", None, true, format = "int32".some),
              "_2" -> RefProperty(ref = "ModelA", required = true))))
    }

    "collect response of case class containing a map of primitive types" in {
      val ra = GET / "test" |>> { () => Ok(ModelMap("asdf", Map("foo"->1))) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = RefProperty("ModelMap",false,None,None,None).some
        )
      )
      sb.collectDefinitions[IO](ra)(Swagger()) must havePair(
        "ModelMap" ->
          ModelImpl(
            id          = prefix + "ModelMap",
            id2         = "ModelMap",
            description = "ModelMap".some,
            `type`      = "object".some,
            properties  =  Map(
            "bar" -> AbstractProperty(`type` = "string", required = true),
            "baz" -> MapProperty(
              required = true,
              additionalProperties = AbstractProperty("integer", format = "int32".some)
            )
          )
        )
      )
    }
  }

  "SwaggerModelsBuilder.collectResponses" should {

    "collect an empty response with response code forbidding entity" in {
      val ra = GET / "test" |>> { () => NoContent.apply }

      sb.collectResponses[IO](ra) must havePair(
        "204" -> Response(description = "No Content", schema = None))
    }

    "collect an empty response with response code allowing entity" in {
      val ra = GET / "test" |>> { () => Ok(()) }

      sb.collectResponses(ra) must havePair(
        "200" -> Response(description = "OK", schema = None))
    }

      "collect response of primitive types" in {
      val ra = GET / "test" |>> { () => Ok("") }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))
    }

    "collect response with file type" in {
      val ra = GET / "test" |>> { () => Ok(SwaggerFileResponse(CsvFile())) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "file").some))
    }

    "collect response of user-defined types" in {
      val ra = GET / "test" |>> { () => Ok(ModelA("", 0)) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(description = "OK", schema = RefProperty(ref = "ModelA").some))
    }

    "collect response of collection of primitive types" in {
      val ra = GET / "test" |>> { () => Ok(List("")) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = AbstractProperty(`type` = "string")).some))
    }

    "collect response of collection of unit" in {
      val ra = GET / "test" |>> { () => Ok(List(())) }

      sb.collectResponses(ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Unit")).some))
    }

    "collect response of map of primitive types" in {
      val ra = GET / "test" |>> { () => Ok(Map("foo"->"bar")) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = MapProperty(additionalProperties = AbstractProperty(`type` = "string")).some))
    }

    "collect response of map of unit" in {
      val ra = GET / "test" |>> { () => Ok(Map(("foo", ()))) }

      sb.collectResponses(ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = MapProperty(additionalProperties = RefProperty(ref = "Unit")).some))
    }

    "collect response of collection of user-defined types" in {
      val ra = GET / "test" |>> { () => Ok(List(ModelA("", 0))) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "ModelA")).some))
    }

    "collect response of tuples" in {
      val ra = GET / "test" |>> { () => Ok((0, ModelA("", 0))) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = RefProperty(ref = "Tuple2«Int,ModelA»").some))
    }

    "collect response of a collection of tuples" in {
      val ra = GET / "test" |>> { () => Ok(List((0, ModelA("", 0)))) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))
    }

    "collect response of a Stream of primitives" in {
      val ra1 = GET / "test" |>> { () => Ok(Stream.eval(IO.pure(""))) }
      val ra2 = GET / "test" |>> { () => Ok(Stream.emit("").covary[IO]) }

      sb.collectResponses[IO](ra1) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))

      sb.collectResponses[IO](ra2) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))
    }

    "collect response of a Stream of non-primitives" in {
      val ra1 = GET / "test" |>> { () => Ok(Stream.eval(IO.pure(List((0, ModelA("", 0)))))) }
      val ra2 = GET / "test" |>> { () => Ok(Stream.emit(List((0, ModelA("", 0)))).covary[IO]) }

      sb.collectResponses[IO](ra1) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))

      sb.collectResponses[IO](ra2) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))
    }

    "collect response of an IO of a primitive" in {
      val ra = GET / "test" |>> { () => Ok(IO.pure("")) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))
    }

    "collect response of an IO of a non-primitive" in {
      val ra = GET / "test" |>> { () => Ok(IO.pure(List((0, ModelA("", 0))))) }

      sb.collectResponses[IO](ra) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))
    }

    "collect multiple responses" in {
      val ra = GET / "test" / pathVar[Int] |>> { (i: Int) =>

        i match {
          case 0 => Ok(List((0, ModelA("A", 0))))
          case _ => Unauthorized("Unauthorized")
        }
      }

      sb.collectResponses[IO](ra) must havePairs(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some),
        "401" -> Response(
          description = "Unauthorized",
          schema      = AbstractProperty(`type` = "string").some))
    }
  }

  implicit def renderableEncoder[F[_], T <: Renderable](implicit F: Applicative[F]): EntityEncoder[F, T] =
    EntityEncoder
      .stringEncoder[F](F, Charset.`UTF-8`)
      .contramap { r: T => "" }
      .withContentType(`Content-Type`(MediaType.`application/json`, Charset.`UTF-8`))

  implicit def tuple2Encoder[F[_], T <: Renderable](implicit F: Applicative[F]): EntityEncoder[F, (Int, T)] =
    EntityEncoder
      .stringEncoder[F](F, Charset.`UTF-8`)
      .contramap { r: (Int, T) => "" }
      .withContentType(`Content-Type`(MediaType.`application/json`, Charset.`UTF-8`))

  implicit def listEntityEncoder[F[_]: Applicative, A]: EntityEncoder[F, List[A]] =
    EntityEncoder.simple[F, List[A]]()(_ => Chunk.bytes("A".getBytes))

  implicit def mapEntityEncoder[F[_]: Applicative, A, B]: EntityEncoder[F, Map[A,B]] =
    EntityEncoder.simple[F, Map[A,B]]()(_ => Chunk.bytes("A".getBytes))

  case class CsvFile()

  object CsvFile {
    implicit def entityEncoderCsvFile: EntityEncoder[IO, CsvFile] =
      EntityEncoder.encodeBy[IO, CsvFile](`Content-Type`(MediaType.`text/csv`, Some(Charset.`UTF-8`))) { file: CsvFile =>
        ByteVector.encodeUtf8("file content").fold(
          IO.raiseError,
          bv => IO.pure(org.http4s.Entity(Stream.emits(bv.toArray), Some(bv.length)))
        )
      }
  }
}
