package org.http4s

import scala.language.implicitConversions

import rho.bits.PathAST._
import rho.bits.HeaderAST._
import rho.bits.QueryAST._

import shapeless.{HNil, ::}
import org.http4s.rho.bits._

import scala.reflect.runtime.universe.TypeTag

package object rho extends Http4s with ResultSyntaxInstances {

  private val stringTag = implicitly[TypeTag[String]]

  implicit def method(m: Method): PathBuilder[HNil] = new PathBuilder(m, PathEmpty)

  implicit def pathMatch(s: String): TypedPath[HNil] = TypedPath(PathMatch(s))

  implicit def pathMatch(s: Symbol): TypedPath[String :: HNil] =
    TypedPath(PathCapture(s.name, StringParser.strParser, stringTag))

  val PathEmpty: PathRule = PathMatch("")

  /**
   * Defines a parameter in query string that should be bound to a route definition.
   * @param name name of the parameter in query
   */
  def param[T](name: String)(implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    TypedQuery(QueryCapture(name, parser, default = None, m))

  def param[T](name: String, default: T, validate: T => Boolean = (_: T) => true)
               (implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    paramR(name, default, {t =>
      if (validate(t)) None
      else Some(SyncRespBuilder.badRequest("Invalid query parameter: \"" + t + "\""))
    })

  /**
   * Defines a parameter in query string that should be bound to a route definition.
   * @param name name of the parameter in query
   * @param default value that should be used if no or an invalid parameter is available
   * @param validate analyze the query and give an optional alternative Response
   */
  def paramR[T](name: String, default: T, validate: T => Option[Response] = (_: T) => None)
                                          (implicit parser: QueryParser[T], m: TypeTag[T]): TypedQuery[T :: HNil] =
    TypedQuery(QueryCapture(name, new ValidatingParser(parser, validate), default = Some(default), m))

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

  /////////////////////////////// Header helpers //////////////////////////////////////

  /* Checks that the header exists */
  def require(header: HeaderKey.Extractable): TypedHeader[HNil] = requireThatR(header)(_ => None)

  /* Check that the header exists and satisfies the condition */
  def requireThat[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Boolean): TypedHeader[HNil] =
    requireThatR(header){ h =>
      if (f(h)) None
      else Some(SyncRespBuilder.badRequest("Invalid header: " + h.value))
    }

  /* Check that the header exists and satisfies the condition */
  def requireThatR[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Option[Response]): TypedHeader[HNil] =
    TypedHeader(HeaderRequire(header, f))

  /** requires the header and will pull this header from the pile and put it into the function args stack */
  def capture[H <: HeaderKey.Extractable](key: H): TypedHeader[H#HeaderT :: HNil] =
    TypedHeader(HeaderCapture(key))

  def requireMap[H <: HeaderKey.Extractable, R](key: H)(f: H#HeaderT => R): TypedHeader[R :: HNil] =
    TypedHeader(HeaderMapper[H, R](key, f))

}
