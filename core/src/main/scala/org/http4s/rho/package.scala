package org.http4s

import cats.syntax.functor._
import cats.{FlatMap, Functor, Monad}
import org.http4s.rho.Result.BaseResult
import org.http4s.rho.{PathBuilder, ResultSyntaxInstances}
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.RequestAST.CaptureRule
import org.http4s.rho.bits._
import org.log4s.getLogger
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import scala.util.control.NonFatal

package object rho extends Http4s {
  type RhoMiddleware[F[_]] = Seq[RhoRoute[F, _ <: HList]] => Seq[RhoRoute[F, _ <: HList]]

  def apply[F[_]]: RhoDsl[F] = new RhoDsl[F] { }
}

trait RhoDsl[F[_]] extends ResultSyntaxInstances[F] with ResponseGeneratorInstances {
  private[this] val logger = getLogger

  private val stringTag = implicitly[TypeTag[String]]

  implicit def method(m: Method): PathBuilder[F, HNil] = new PathBuilder(m, PathEmpty)

  implicit def pathMatch(s: String): TypedPath[F, HNil] = TypedPath(PathMatch(s))

  implicit def pathMatch(s: Symbol): TypedPath[F, String :: HNil] =
    TypedPath(PathCapture(s.name, None, StringParser.strParser, stringTag))

  val PathEmpty: PathRule = PathMatch("")

  /**
   * Defines a parameter in query string that should be bound to a route definition.
   * @param name name of the parameter in query
   */
  def param[T](name: String)(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, None, _ => None)

  /**
    * Defines a parameter in query string that should be bound to a route definition.
    * @param name name of the parameter in query
    * @param description description of the parameter
    */
  def paramD[T](name: String, description: String)(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), None, _ => None)

  /** Define a query parameter with a default value */
  def param[T](name: String, default: T)(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, Some(default), _ => None)

  /** Define a query parameter with description and a default value */
  def paramD[T](name: String, default: T, description: String)(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), Some(default), _ => None)

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def param[T](name: String, validate: T => Boolean)
                    (implicit F: Monad[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramR(name, {t =>
      if (validate(t)) None
      else Some(BadRequest[F](s"""Invalid query parameter: "$name" = "$t"""").widen)
    })

  /** Define a query parameter with description that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def paramD[T](name: String, description: String, validate: T => Boolean)
                     (implicit F: Monad[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramRDescr(name, description, { t: T =>
      if (validate(t)) None
      else Some(BadRequest[F](s"""Invalid query parameter: "$name" = "$t"""").widen)
    })

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def param[T](name: String, default: T, validate: T => Boolean)
                    (implicit F: Monad[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramR(name, default, { t: T =>
      if (validate(t)) None
      else Some(BadRequest[F](s"""Invalid query parameter: "$name" = "$t"""").widen)
    })

  /** Define a query parameter with description that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def paramD[T](name: String, description: String, default: T, validate: T => Boolean)
                     (implicit F: Monad[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramR(name, description, default, { t =>
      if (validate(t)) None
      else Some(BadRequest[F](s"""Invalid query parameter: "$name" = "$t"""").widen)
    })

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String, validate: T => Option[F[BaseResult[F]]])(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, None, validate)

  /** Defines a parameter in query string with description that should be bound to a route definition. */
  def paramRDescr[T](name: String, description: String, validate: T => Option[F[BaseResult[F]]])(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), None, validate)

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String, default: T, validate: T => Option[F[BaseResult[F]]])(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, Some(default), validate)

  /** Defines a parameter in query string with description that should be bound to a route definition. */
  def paramR[T](name: String, description: String, default: T, validate: T => Option[F[BaseResult[F]]])(implicit F: FlatMap[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), Some(default), validate)

  /** Create a query capture rule using the `Request`'s `Uri`
    *
    * @param f function generating the result or failure
    */
  def genericQueryCapture[R](f: Query => ResultResponse[F, R]): TypedQuery[F, R :: HNil] =
    genericRequestQueryCapture[R](req => f(req.uri.query))

  /** Create a query capture rule using the `Request`
    *
    * In general, this function should be avoided for most cases because it has access to the entire `Request`
    * which allows it to modify the `Request` body which should be avoided.
    *
    * @param f function generating the result or failure
    */
  def genericRequestQueryCapture[R](f: Request[F] => ResultResponse[F, R]): TypedQuery[F, R :: HNil] =
    TypedQuery(CaptureRule(f))

  /////////////////////////////// Path helpers //////////////////////////////////////
  /**
   * Defines a path variable of a URI that should be bound to a route definition
   */
  def pathVar[T](implicit parser: StringParser[F, String], m: TypeTag[T]): TypedPath[F, T :: HNil] =
    pathVar(m.tpe.toString.toLowerCase)(parser, m)

  /**
   * Defines a path variable of a URI that should be bound to a route definition
   */
  def pathVar[T](id: String)(implicit parser: StringParser[F, String], m: TypeTag[T]): TypedPath[F, T :: HNil] =
    TypedPath(PathCapture[F](id, None, parser, stringTag))

  /**
    * Defines a path variable of a URI with description that should be bound to a route definition
    */
  def pathVar[T](id: String, description: String)(implicit parser: StringParser[F, String], m: TypeTag[T]): TypedPath[F, T :: HNil] =
    TypedPath(PathCapture[F](id, Some(description), parser, stringTag))

  /**
   * Helper to be able to define a path with one level only.
   * {{{
   * val hello = Root / "hello"
   * }}}
   */
  def root(): TypedPath[F, HNil] = TypedPath(PathEmpty)

  def * : CaptureTail.type = CaptureTail

  /////////////////////////////// Header helpers //////////////////////////////////////

  /** Requires that the header exists
    *
    * @param header `HeaderKey` that identifies the header which is required
    */
  def exists(header: HeaderKey.Extractable)(implicit F: Monad[F]): TypedHeader[F, HNil] = existsAndR(header)(_ => None)

  /** Requires that the header exists and satisfies the condition
    *
    * @param header  `HeaderKey` that identifies the header to capture and parse
    * @param f predicate function where a return value of `false` signals an invalid
    *          header and aborts evaluation with a _BadRequest_ response.
    */
  def existsAnd[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Boolean)(implicit F: Monad[F]): TypedHeader[F, HNil] =
    existsAndR[H](header){ h =>
      if (f(h)) None
      else Some(BadRequest[F](s"Invalid header: ${h.name} = ${h.value}").widen)
    }

  /** Check that the header exists and satisfies the condition
    *
    * @param header `HeaderKey` that identifies the header to capture and parse
    * @param f function that evaluates the header and returns a Some(Response) to
    *          immediately send back to the user or None to continue evaluation.
    */
  def existsAndR[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Option[F[BaseResult[F]]])(implicit F: Monad[F]): TypedHeader[F, HNil] =
    captureMapR(header, None){ h => f(h) match {
        case Some(r) => Left(r)
        case None    => Right(())
      }
    }.ignore


  /** requires the header and will pull this header from the pile and put it into the function args stack */
  def capture[H <: HeaderKey.Extractable](key: H)(implicit F: Monad[F]): TypedHeader[F, H#HeaderT :: HNil] =
    captureMap(key)(identity)

  /** Capture a specific header and map its value
    *
    * @param key `HeaderKey` used to identify the header to capture
    * @param f mapping function
    */
  def captureMap[H <: HeaderKey.Extractable, R](key: H)(f: H#HeaderT => R)(implicit F: Monad[F]): TypedHeader[F, R :: HNil] =
    captureMapR(key, None)(f andThen (Right(_)))

  /** Capture a specific header and map its value with an optional default
    *
    * @param key `HeaderKey` used to identify the header to capture
    * @param default optional default for the case of a missing header
    * @param f mapping function
    */
  def captureMapR[H <: HeaderKey.Extractable, R](key: H, default: Option[F[BaseResult[F]]] = None)(f: H#HeaderT => Either[F[BaseResult[F]], R])(implicit F: Monad[F]): TypedHeader[F, R :: HNil] =
    _captureMapR(key, default)(f)

  /** Create a header capture rule using the `Request`'s `Headers`
    *
    * @param f function generating the result or failure
    */
  def genericHeaderCapture[R](f: Headers => ResultResponse[F, R]): TypedHeader[F, R :: HNil] =
    genericRequestHeaderCapture[R](req => f(req.headers))

  /** Create a header capture rule using the `Request`
    *
    * In general, this function should be avoided for most cases because it has access to the entire `Request`
    * which allows it to modify the `Request` body which should be avoided.
    *
    * @param f function generating the result or failure
    */
  def genericRequestHeaderCapture[R](f: Request[F] => ResultResponse[F, R]): TypedHeader[F, R :: HNil] =
    TypedHeader[F, R :: HNil](CaptureRule(f))

  /** Defines a parameter in query string that should be bound to a route definition. */
  private def _paramR[T](name: String, description: Option[String], default: Option[T], validate: T => Option[F[BaseResult[F]]])(implicit F: Functor[F], parser: QueryParser[F, T], m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    genericRequestQueryCapture[T] { req =>
        val result = parser.collect(name, req.uri.multiParams, default)
        result.flatMap { r => validate(r) match {
          case None       => result
          case Some(resp) => FailureResponse.pure(F.map(resp)(_.resp))
        }
      }
    }.withMetadata(QueryMetaData(name, description, parser, default = default, m))

  private def _captureMapR[H <: HeaderKey.Extractable, R](key: H, default: Option[F[BaseResult[F]]])(f: H#HeaderT => Either[F[BaseResult[F]], R])(implicit F: Monad[F]): TypedHeader[F, R :: HNil] =
    genericHeaderCapture[R] { headers =>
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
