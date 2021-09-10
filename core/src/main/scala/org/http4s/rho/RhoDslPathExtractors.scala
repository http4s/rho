package org.http4s.rho

import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits._
import org.http4s.rho.RhoDslPathExtractors._
import shapeless.{::, HNil}

import scala.reflect.runtime.universe.TypeTag

trait RhoDslPathExtractors[F[_]] {

  implicit def pathMatch(s: String): TypedPath[F, HNil] = TypedPath(PathMatch(s))

  /** Provides 'pathVar syntax for String path variables (Scala 2.12 only)
    */
  implicit def pathCapture(s: Symbol): TypedPath[F, String :: HNil] =
    TypedPath(PathCapture(s.name, None, StringParser.strParser, stringTag))

  /** Provides pv"pathVarName" syntax for String path variables as an alternative for 'pathVar
    * (Symbol) syntax which was removed in Scala 2.13.
    */
  implicit def pathCapture(sc: StringContext): PathCaptureStringContext[F] =
    new PathCaptureStringContext[F](sc)

  /** Defines a path variable of a URI that should be bound to a route definition
    */
  def pathVar[T](implicit parser: StringParser[F, T], m: TypeTag[T]): TypedPath[F, T :: HNil] =
    pathVar(m.tpe.toString.toLowerCase)(parser)

  /** Defines a path variable of a URI that should be bound to a route definition
    */
  def pathVar[T](id: String)(implicit parser: StringParser[F, T]): TypedPath[F, T :: HNil] =
    TypedPath(PathCapture[F](id, None, parser, stringTag))

  /** Defines a path variable of a URI with description that should be bound to a route definition
    */
  def pathVar[T](id: String, description: String)(implicit
      parser: StringParser[F, T]): TypedPath[F, T :: HNil] =
    TypedPath(PathCapture[F](id, Some(description), parser, stringTag))

}

object RhoDslPathExtractors {

  private val stringTag = implicitly[TypeTag[String]]

  class PathCaptureStringContext[F[_]](val sc: StringContext) extends AnyVal {
    def pv(): TypedPath[F, String :: HNil] =
      TypedPath[F, String :: HNil](
        PathCapture(sc.parts.mkString, None, StringParser.strParser, stringTag)
      )
  }
}
