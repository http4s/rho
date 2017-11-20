package org.http4s

import org.http4s.rho.Result.BaseResult
import org.http4s.rho.bits.RequestAST.CaptureRule
import org.http4s.rho.bits.ResponseGeneratorInstances.BadRequest

import scala.language.implicitConversions

import rho.bits.PathAST._

import shapeless.{HList, HNil, ::}
import org.http4s.rho.bits._

import scala.reflect.runtime.universe.TypeTag
import scala.util.control.NonFatal
import cats.effect.IO

import org.log4s.getLogger

package object rho extends Http4s with ResultSyntaxInstances {

  private[this] val logger = getLogger

  object dsl extends bits.MethodAliases with bits.ResponseGeneratorInstances

  type RhoMiddleware = Seq[RhoRoute[_ <: HList]] => Seq[RhoRoute[_ <: HList]]

  private val stringTag = implicitly[TypeTag[String]]

  implicit def method(m: Method): PathBuilder[HNil] = new PathBuilder(m, PathEmpty)

  implicit def pathMatch(s: String): TypedPath[HNil] = TypedPath(PathMatch(s))

  implicit def pathMatch(s: Symbol): TypedPath[String :: HNil] =
    TypedPath(PathCapture(s.name, None, StringParser.strParser, stringTag))

  val PathEmpty: PathRule = PathMatch("")

  /**
   * Defines a parameter in query string that should be bound to a route definition.
   * @param name name of the parameter in query
   */
  def param[T](name: String)(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, None, None, _ => None)

  /**
    * Defines a parameter in query string that should be bound to a route definition.
    * @param name name of the parameter in query
    * @param description description of the parameter
    */
  def paramD[T](name: String, description: String)(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, Some(description), None, _ => None)

  /** Define a query parameter with a default value */
  def param[T](name: String, default: T)(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, None, Some(default), _ => None)

  /** Define a query parameter with description and a default value */
  def paramD[T](name: String, default: T, description: String)(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, Some(description), Some(default), _ => None)

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def param[T](name: String, validate: T => Boolean)
               (implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    paramR(name, {t =>
      if (validate(t)) None
      else Some(BadRequest(s"""Invalid query parameter: "$name" = "$t""""))
    })

  /** Define a query parameter with description that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def paramD[T](name: String, description: String, validate: T => Boolean)
              (implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    paramR(name, description, { t: T =>
      if (validate(t)) None
      else Some(BadRequest(s"""Invalid query parameter: "$name" = "$t""""))
    })

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def param[T](name: String, default: T, validate: T => Boolean)
              (implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    paramR(name, default, { t: T =>
      if (validate(t)) None
      else Some(BadRequest(s"""Invalid query parameter: "$name" = "$t""""))
    })

  /** Define a query parameter with description that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def paramD[T](name: String, description: String, default: T, validate: T => Boolean)
              (implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    paramR(name, description, default, { t =>
      if (validate(t)) None
      else Some(BadRequest(s"""Invalid query parameter: "$name" = "$t""""))
    })

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String, validate: T => Option[IO[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, None, None, validate)

  /** Defines a parameter in query string with description that should be bound to a route definition. */
  def paramR[T](name: String, description: String, validate: T => Option[IO[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, Some(description), None, validate)

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String, default: T, validate: T => Option[IO[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, None, Some(default), validate)

  /** Defines a parameter in query string with description that should be bound to a route definition. */
  def paramR[T](name: String, description: String, default: T, validate: T => Option[IO[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    _paramR(name, Some(description), Some(default), validate)

  /** Create a query capture rule using the `Request`'s `Uri`
    *
    * @param f function generating the result or failure
    */
  def genericQueryCapture[R](f: Query => ResultResponse[R]): TypedQuery[R :: HNil] =
    genericRequestQueryCapture(req => f(req.uri.query))

  /** Create a query capture rule using the `Request`
    *
    * In general, this function should be avoided for most cases because it has access to the entire `Request`
    * which allows it to modify the `Request` body which should be avoided.
    *
    * @param f function generating the result or failure
    */
  def genericRequestQueryCapture[R](f: Request[IO] => ResultResponse[R]): TypedQuery[R :: HNil] =
    TypedQuery(CaptureRule(f))

  /////////////////////////////// Path helpers //////////////////////////////////////
  /**
   * Defines a path variable of a URI that should be bound to a route definition
   */
  def pathVar[T](implicit parser: StringParser[T], m: TypeTag[T]): TypedPath[T :: HNil] =
    pathVar(m.tpe.toString.toLowerCase)(parser, m)

  /**
   * Defines a path variable of a URI that should be bound to a route definition
   */
  def pathVar[T](id: String)(implicit parser: StringParser[T], m: TypeTag[T]): TypedPath[T :: HNil] =
    TypedPath(PathCapture(id, None, parser, stringTag))

  /**
    * Defines a path variable of a URI with description that should be bound to a route definition
    */
  def pathVar[T](id: String, description: String)(implicit parser: StringParser[T], m: TypeTag[T]): TypedPath[T :: HNil] =
    TypedPath(PathCapture(id, Some(description), parser, stringTag))

  /**
   * Helper to be able to define a path with one level only.
   * {{{
   * val hello = Root / "hello"
   * }}}
   */
  def root(): TypedPath[HNil] = TypedPath(PathEmpty)

  def * : CaptureTail.type = CaptureTail

  /////////////////////////////// Header helpers //////////////////////////////////////

  /** Requires that the header exists
    *
    * @param header `HeaderKey` that identifies the header which is required
    */
  def exists(header: HeaderKey.Extractable): TypedHeader[HNil] = existsAndR(header)(_ => None)

  /** Requires that the header exists and satisfies the condition
    *
    * @param header  `HeaderKey` that identifies the header to capture and parse
    * @param f predicate function where a return value of `false` signals an invalid
    *          header and aborts evaluation with a _BadRequest_ response.
    */
  def existsAnd[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Boolean): TypedHeader[HNil] =
    existsAndR(header){ h =>
      if (f(h)) None
      else Some(BadRequest(s"Invalid header: ${h.name} = ${h.value}"))
    }

  /** Check that the header exists and satisfies the condition
    *
    * @param header `HeaderKey` that identifies the header to capture and parse
    * @param f function that evaluates the header and returns a Some(Response) to
    *          immediately send back to the user or None to continue evaluation.
    */
  def existsAndR[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Option[IO[BaseResult]]): TypedHeader[HNil] =
    captureMapR(header, None){ h => f(h) match {
        case Some(r) => Left(r)
        case None    => Right(())
      }
    }.ignore


  /** requires the header and will pull this header from the pile and put it into the function args stack */
  def capture[H <: HeaderKey.Extractable](key: H): TypedHeader[H#HeaderT :: HNil] =
    captureMap(key)(identity)

  /** Capture a specific header and map its value
    *
    * @param key `HeaderKey` used to identify the header to capture
    * @param f mapping function
    */
  def captureMap[H <: HeaderKey.Extractable, R](key: H)(f: H#HeaderT => R): TypedHeader[R :: HNil] =
    captureMapR(key, None)(f andThen (Right(_)))

  /** Capture a specific header and map its value with an optional default
    *
    * @param key `HeaderKey` used to identify the header to capture
    * @param default optional default for the case of a missing header
    * @param f mapping function
    */
  def captureMapR[H <: HeaderKey.Extractable, R](key: H, default: Option[IO[BaseResult]] = None)(f: H#HeaderT => Either[IO[BaseResult], R]): TypedHeader[R :: HNil] =
    _captureMapR(key, default)(f)

  /** Create a header capture rule using the `Request`'s `Headers`
    *
    * @param f function generating the result or failure
    */
  def genericHeaderCapture[R](f: Headers => ResultResponse[R]): TypedHeader[R :: HNil] =
    genericRequestHeaderCapture(req => f(req.headers))

  /** Create a header capture rule using the `Request`
    *
    * In general, this function should be avoided for most cases because it has access to the entire `Request`
    * which allows it to modify the `Request` body which should be avoided.
    *
    * @param f function generating the result or failure
    */
  def genericRequestHeaderCapture[R](f: Request[IO] => ResultResponse[R]): TypedHeader[R :: HNil] =
    TypedHeader(CaptureRule(f))

  /** Defines a parameter in query string that should be bound to a route definition. */
  private def _paramR[T](name: String, description: Option[String], default: Option[T], validate: T => Option[IO[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    genericRequestQueryCapture { req =>
        val result = parser.collect(name, req.uri.multiParams, default)
        result.flatMap { r => validate(r) match {
          case None       => result
          case Some(resp) => FailureResponse.pure(resp.map(_.resp))
        }
      }
    }.withMetadata(QueryMetaData(name, description, parser, default = default, m))

  private def _captureMapR[H <: HeaderKey.Extractable, R](key: H, default: Option[IO[BaseResult]])(f: H#HeaderT => Either[IO[BaseResult], R]): TypedHeader[R :: HNil] =
    genericHeaderCapture { headers =>
      headers.get(key) match {
        case Some(h) =>
          try f(h) match {
            case Right(r) => SuccessResponse(r)
            case Left(r) => FailureResponse.result(r)
          } catch {
            case NonFatal(e) =>
              logger.error(e)(s"""Failure during header capture: "${key.name}" = "${h.value}"""")
              FailureResponse.error("Error processing request.")
          }

        case None => default match {
          case Some(r) => FailureResponse.result(r)
          case None    => FailureResponse.badRequest(s"Missing header: ${key.name}")
        }
      }
    }.withMetadata(HeaderMetaData(key, default.isDefined))
}
