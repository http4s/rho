package org.http4s.rho

import cats.syntax.functor._
import cats.{FlatMap, Functor, Monad}
import org.http4s._
import org.http4s.rho.Result.BaseResult
import org.http4s.rho.bits.RequestAST.CaptureRule
import org.http4s.rho.bits._
import shapeless.{::, HNil}

import scala.reflect.runtime.universe.TypeTag

trait RhoDslQueryParamExtractors[F[_]] extends FailureResponseOps[F] {

  /** Defines a parameter in query string that should be bound to a route definition. */
  private def _paramR[T](
      name: String,
      description: Option[String],
      default: Option[T],
      validate: T => Option[F[BaseResult[F]]],
      enums: Seq[String])(implicit
      F: Functor[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]
  ): TypedQuery[F, T :: HNil] =
    genericRequestQueryCapture[T] { req =>
      val result = parser.collect(name, req.uri.multiParams, default)
      result.flatMap { r =>
        validate(r) match {
          case None => result
          case Some(resp) => FailureResponse.pure(F.map(resp)(_.resp))
        }
      }
    }.withMetadata(QueryMetaData(name, description, parser, default = default, m, enums))

  /** Defines a parameter in query string that should be bound to a route definition.
    *
    * @param name name of the parameter in query
    */
  def param[T](name: String)(implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, None, _ => None, Nil)

  /** Defines a parameter in query string that should be bound to a route definition.
    *
    * @param name        name of the parameter in query
    * @param description description of the parameter
    */
  def paramD[T](name: String, description: String)(implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), None, _ => None, Nil)

  /** Define a query parameter with a default value */
  def param[T](name: String, default: T)(implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, Some(default), _ => None, Nil)

  /** Define a query parameter with description and a default value */
  def paramD[T](name: String, default: T, description: String)(implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), Some(default), _ => None, Nil)

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response.
    */
  def param[T](name: String, validate: T => Boolean)(implicit
      F: Monad[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramR(
      name,
      t =>
        if (validate(t)) None
        else Some(invalidQueryParameterResponse(name, t))
    )

  /** Define a query parameter with description that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response.
    */
  def paramD[T](name: String, description: String, validate: T => Boolean)(implicit
      F: Monad[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramRDescr(
      name,
      description,
      { t: T =>
        if (validate(t)) None
        else Some(invalidQueryParameterResponse(name, t))
      }
    )

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response.
    */
  def param[T](name: String, default: T, validate: T => Boolean)(implicit
      F: Monad[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramR(
      name,
      default,
      { t: T =>
        if (validate(t)) None
        else Some(invalidQueryParameterResponse(name, t))
      }
    )

  /** Define a query parameter with description that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response.
    */
  def paramD[T](name: String, description: String, default: T, validate: T => Boolean)(implicit
      F: Monad[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    paramR(
      name,
      description,
      default,
      t =>
        if (validate(t)) None
        else Some(invalidQueryParameterResponse(name, t))
    )

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String, validate: T => Option[F[BaseResult[F]]])(implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, None, validate, Nil)

  /** Defines a parameter in query string with description that should be bound to a route definition. */
  def paramRDescr[T](name: String, description: String, validate: T => Option[F[BaseResult[F]]])(
      implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), None, validate, Nil)

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String, default: T, validate: T => Option[F[BaseResult[F]]])(implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, Some(default), validate, Nil)

  /** Defines a parameter in query string with description that should be bound to a route definition. */
  def paramR[T](
      name: String,
      description: String,
      default: T,
      validate: T => Option[F[BaseResult[F]]])(implicit
      F: FlatMap[F],
      parser: QueryParser[F, T],
      m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, Some(description), Some(default), validate, Nil)

  /** Defines an enum parameter in query string that should be bound to a route definition.
    *
    * @param name name of the parameter in query
    * @param default default value
    * @param enums list of allowed values
    */
  def paramE[T](name: String, default: T, enums: Seq[String])(implicit
    F: FlatMap[F],
    parser: QueryParser[F, T],
    m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, Some(default), _ => None, enums)


  /** Defines an enum parameter in query string that should be bound to a route definition.
    *
    * @param name name of the parameter in query
    * @param enums list of allowed values
    */
  def paramE[T](name: String, enums: Seq[String])(implicit
    F: FlatMap[F],
    parser: QueryParser[F, T],
    m: TypeTag[T]): TypedQuery[F, T :: HNil] =
    _paramR(name, None, None, _ => None, enums)

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
  def genericRequestQueryCapture[R](
      f: Request[F] => ResultResponse[F, R]): TypedQuery[F, R :: HNil] =
    TypedQuery(CaptureRule(f))

  protected def invalidQueryParameterResponse[T](name: String, value: T)(implicit
      F: Monad[F]): F[BaseResult[F]] =
    BadRequest(s"""Invalid query parameter: "$name" = "$value"""").widen
}
