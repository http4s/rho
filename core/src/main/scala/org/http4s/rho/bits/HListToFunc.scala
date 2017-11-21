package org.http4s
package rho.bits

import org.http4s.rho.Action
import shapeless.{HNil, ::, HList}
import cats.effect.IO

/** Converter of an value of type F to the HList of type T
  *
 * @tparam T HList type of the incoming values
 * @tparam F type of element onto which T will be mapped
 */
trait HListToFunc[T <: HList, -F] {
  def toAction(f: F): Action[T]
}

object HListToFunc {
  import shapeless._
  import shapeless.ops.hlist._
  import shapeless.ops.function._
  import shapeless.syntax.std.function._

  /** Converter of any type with a result matcher when there are no values on the stack
    *
    * @tparam R type of result
    */
  implicit def const0[R](implicit m: ResultMatcher[R]): HListToFunc[HNil, R] = new MatcherHListToFunc[HNil,R] {
    override def matcher = m
    override def conv(r: R): (Request[IO], HNil) => IO[Response[IO]] = (req, _) => m.conv(req, r)
  }

  /** Converter for types `FunctionN` to an `HList` */
  implicit def instance[T <: HList, TR <: HList, F, R](implicit fp: FnToProduct.Aux[F, TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[R]]): HListToFunc[T, F] = new MatcherHListToFunc[T, F] {
    override val matcher = m.value
    override def conv(f: F): (Request[IO], T) => IO[Response[IO]] = (req: Request[IO], h: T) => { matcher.conv(req, f.toProduct(rev(h))) }
  }

  /** Converter for types `FunctionN` where the first element is a `Request` to an `HList` */
  implicit def instance1[T <: HList, TR <: HList, F, R](implicit fp: FnToProduct.Aux[F, Request[IO] :: TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[R]]): HListToFunc[T, F] = new MatcherHListToFunc[T, F] {
    override val matcher = m.value
    override def conv(f: F): (Request[IO], T) => IO[Response[IO]] = (req: Request[IO], h: T) => { matcher.conv(req, f.toProduct(req :: rev(h))) }
  }

  // for convenience
  private trait MatcherHListToFunc[T <: HList, -F] extends HListToFunc[T, F] {
    protected def matcher: ResultMatcher[_]
    protected def conv(f: F): (Request[IO], T) => IO[Response[IO]]
    final override def toAction(f: F) = Action(matcher.resultInfo, matcher.encodings, conv(f))
  }
}
