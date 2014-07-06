package org.http4s

import scala.language.existentials
import scala.language.implicitConversions

import rho.bits.PathAST._
import rho.bits.HeaderAST._
import rho.bits.QueryAST._

import shapeless.{ HNil, :: }
import org.http4s.rho.bits._


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
  
  def ROOT = PathEmpty

  implicit def method(m: Method): PathBuilder[HNil] = new PathBuilder(m, PathEmpty)

  implicit def pathMatch(s: String): PathRule[HNil] = PathMatch(s)

  implicit def pathMatch(s: Symbol): PathRule[String::HNil] =
    PathCapture(s.name, StringParser.strParser)

  def query[T](key: String, default: T)(implicit parser: QueryParser[T], m: Manifest[T]): QueryCapture[T] =
    query(key, Some(default))

  def query[T](key: String, default: Option[T] = None)(implicit parser: QueryParser[T], m: Manifest[T]): QueryCapture[T] =
    QueryCapture(key, parser, default)

  def pathVar[T](id: String = "unnamed")(implicit parser: StringParser[T], m: Manifest[T]) =
    PathCapture(id, parser)

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
