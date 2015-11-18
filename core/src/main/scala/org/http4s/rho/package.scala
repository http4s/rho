package org.http4s

import org.http4s.rho.Result.BaseResult
import org.http4s.rho.bits.QueryReader._
import org.http4s.rho.bits.ResponseGeneratorInstances.BadRequest

import scala.language.implicitConversions

import rho.bits.PathAST._
import rho.bits.HeaderAST._
import rho.bits.QueryAST._

import shapeless.{HList, HNil, ::}
import org.http4s.rho.bits._

import scala.reflect.runtime.universe.TypeTag
import scalaz.{\/, \/-}
import scalaz.concurrent.Task

package object rho extends Http4s with ResultSyntaxInstances {

  object dsl extends bits.MethodAliases with bits.ResponseGeneratorInstances

  type RhoMiddleware = Seq[RhoRoute[_ <: HList]] => Seq[RhoRoute[_ <: HList]]

  implicit def method(m: Method): PathBuilder[HNil] = new PathBuilder(m, PathEmpty)

  implicit def pathMatch(s: String): TypedPath[HNil] = TypedPath(PathMatch(s))

  implicit def pathMatch(s: Symbol): TypedPath[String :: HNil] =
    TypedPath(PathCapture(s.name, StringParser.strParser, stringTag))

  val PathEmpty: PathRule = PathMatch("")

  /////////////////// begin Query construction helpers ////////////////////////////////////////////

  /**
   * Defines a parameter in query string that should be bound to a route definition.
   * @param name name of the parameter in query
   */
  def param[T](name: String)(implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] =
    _param(name, None)

  /** Define a query parameter with a default value */
  def param[T](name: String, default: T)(implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] =
    _param(name, Some(default))

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def paramV[T](name: String)(validate: T => Boolean)(implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] =
    paramR(name){ t =>
      if (validate(t)) None
      else Some(BadRequest("Invalid query parameter: \"" + t + "\""))
    }

  /** Define a query parameter that will be validated with the predicate
    *
    * Failure of the predicate results in a '403: BadRequest' response. */
  def paramV[T](name: String, default: T)(validate: T => Boolean)
              (implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] =
    paramR(name, default){ t =>
      if (validate(t)) None
      else Some(BadRequest("Invalid query parameter: \"" + t + "\""))
    }

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String)(validate: T => Option[Task[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] =
    _paramR(name, None, validate)

  /** Defines a parameter in query string that should be bound to a route definition. */
  def paramR[T](name: String, default: T)(validate: T => Option[Task[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] =
    _paramR(name, Some(default), validate)

  /////////////////// End Query construction helpers //////////////////////////////////////////////

  /////////////////// Begin path construction helpers /////////////////////////////////////////////

  /**
   * Defines a path variable of a URI that should be bound to a route definition
   */
  def pathVar[T](implicit parser: StringParser[T], m: TypeTag[T]): TypedPath[T :: HNil] =
    pathVar(m.tpe.toString.toLowerCase)(parser, m)

  /**
   * Defines a path variable of a URI that should be bound to a route definition
   */
  def pathVar[T](id: String)(implicit parser: StringParser[T], m: TypeTag[T]): TypedPath[T :: HNil] =
    TypedPath(PathCapture(id, parser, stringTag))

  /**
   * Helper to be able to define a path with one level only.
   * {{{
   * val hello = Root / "hello"
   * }}}
   */
  def root(): TypedPath[HNil] = TypedPath(PathEmpty)

  def * : CaptureTail.type = CaptureTail

  /////////////////// End Path construction helpers ///////////////////////////////////////////////

  /////////////////// Begin Header construction helpers ///////////////////////////////////////////

  /* Checks that the header exists */
  def exists(header: HeaderKey.Extractable): TypedHeader[HNil] = existsAndR(header)(_ => None)

  /* Check that the header exists and satisfies the condition */
  def existsAnd[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Boolean): TypedHeader[HNil] =
    existsAndR(header){ h =>
      if (f(h)) None
      else Some(BadRequest("Invalid header: " + h.value))
    }

  /* Check that the header exists and satisfies the condition */
  def existsAndR[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Option[Task[BaseResult]]): TypedHeader[HNil] =
    TypedHeader(HeaderExists(header, f))


  /** requires the header and will pull this header from the pile and put it into the function args stack */
  def capture[H <: HeaderKey.Extractable](key: H): TypedHeader[H#HeaderT :: HNil] =
    captureMap(key)(identity)

  def captureMap[H <: HeaderKey.Extractable, R](key: H)(f: H#HeaderT => R): TypedHeader[R :: HNil] =
    TypedHeader(HeaderCapture[H, R](key, f.andThen(\/-(_)), None))

  def captureMapR[H <: HeaderKey.Extractable, R](key: H, default: Option[Task[BaseResult]] = None)(f: H#HeaderT => Task[BaseResult]\/R): TypedHeader[R :: HNil] =
    TypedHeader(HeaderCapture[H, R](key, f, default))

  /////////////////// End Header construction helpers /////////////////////////////////////////////

  // Misc utilities ///////////////////////////////////////////////////////////////////////////////
  private val stringTag = implicitly[TypeTag[String]]

  /** Define a query parameter with a default value */
  private def _param[T](name: String, default: Option[T])(implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] = {
    val captureParams = QueryCaptureParams(name, true, m)
//    ExtractingReader[T](req => parser.collect(name, req.uri.multiParams, default), captureParams)
    QueryReader(captureParams::Nil)(parser.collect(name, _, default))
  }

  private def _paramR[T](name: String, default: Option[T], validate: T => Option[Task[BaseResult]])(implicit parser: QueryParser[T], m: TypeTag[T]): QueryReader[T] =  {
    val captureParams = QueryCaptureParams(name, true, m)

    QueryReader(captureParams::Nil){ params =>
      val result = parser.collect(name, params, default)
      result.flatMap { t =>
        validate(t) match {
          case None       => result
          case Some(resp) => FailureResponse.pure(resp.map(_.resp))
        }
      }
    }
  }
}
