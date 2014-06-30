package org.http4s

import scala.language.existentials

import rho.bits.PathAST._
import rho.bits.HeaderAST._

import shapeless.{ HNil, :: }
import org.http4s.rho.bits._

/**
 * Created by Bryce Anderson on 4/28/14.
 */
package object rho {

  val OPTIONS = Method.Options
  val GET = Method.Get
  val HEAD = Method.Head
  val POST = Method.Post
  val PUT = Method.Put
  val DELETE = Method.Delete
  val TRACE = Method.Trace
  val CONNECT = Method.Connect
  val PATCH = Method.Patch

  implicit def method(m: Method): PathBuilder[HNil] = new PathBuilder(m, PathEmpty)

  implicit def pathMatch(s: String): CombinablePathRule[HNil] = PathMatch(s)

  implicit def pathMatch(s: Symbol): CombinablePathRule[String :: HNil] =
    PathCapture(StringParser.strParser, Some(s"Param name: ${s.name}"))

  def query[T](key: String, default: T)(implicit parser: QueryParser[T], m: Manifest[T]) = QueryRule[T](key, parser, Some(default))

  def query[T](key: String, default: Option[T] = None)(implicit parser: QueryParser[T], m: Manifest[T]) = QueryRule[T](key, parser, default)

  def pathVar[T](implicit parser: StringParser[T], m: Manifest[T]) = PathCapture(parser, None)

  def pathVar[T](id: String)(implicit parser: StringParser[T], m: Manifest[T]) = PathCapture(parser, Some(id))

  def * = CaptureTail()

  /////////////////////////////// Header helpers //////////////////////////////////////

  /* Checks that the header exists */
  def require(header: HeaderKey.Extractable): HeaderRule[HNil] = requireThat(header)(_ => true)

  /* Check that the header exists and satisfies the condition */
  def requireThat[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Boolean = { _: H#HeaderT => true }): HeaderRule[HNil] =
    HeaderRequire[H](header, f)

  /** requires the header and will pull this header from the pile and put it into the function args stack */
  def capture[H <: HeaderKey.Extractable](key: H): HeaderRule[H#HeaderT :: HNil] =
    HeaderCapture[H#HeaderT](key)

  def requireMap[H <: HeaderKey.Extractable, R](key: H)(f: H#HeaderT => R): HeaderRule[R :: HNil] =
    HeaderMapper[H, R](key, f)
}
