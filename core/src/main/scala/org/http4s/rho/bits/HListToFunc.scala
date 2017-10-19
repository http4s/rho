package org.http4s
package rho.bits

import cats.Monad
import org.http4s.rho.Action
import shapeless.HList

/** Converter of an value of type F to the HList of type T
  *
 * @tparam T HList type of the incoming values
 * @tparam FU type of element onto which T will be mapped
 */
trait HListToFunc[F[_], T <: HList, -FU] {
  def toAction(f: FU): Action[F, T]
}

object HListToFunc {
  import shapeless._
  import shapeless.ops.function._
  import shapeless.ops.hlist._
  import shapeless.syntax.std.function._

  /** Converter of any type with a result matcher when there are no values on the stack
    *
    * @tparam R type of result
    */
  implicit def const0[F[_], R](implicit F: Monad[F], m: ResultMatcher[F, R], w: EntityEncoder[F, R]): HListToFunc[F, HNil, R] = new MatcherHListToFunc[F, HNil, R] {
    override def matcher: ResultMatcher[F, R] = m
    override def conv(r: R): (Request[F], HNil) => F[Response[F]] = (req, _) => m.conv(req, r)
  }

  /** Converter for types `FunctionN` to an `HList` */
  implicit def instance[F[_], T <: HList, TR <: HList, FU, R](implicit F: Monad[F], w: EntityEncoder[F, R], fp: FnToProduct.Aux[FU, TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[F, R]]): HListToFunc[F, T, FU] = new MatcherHListToFunc[F, T, FU] {
    override def matcher: ResultMatcher[F, R] = m.value
    override def conv(f: FU): (Request[F], T) => F[Response[F]] = (req: Request[F], h: T) => { matcher.conv(req, f.toProduct(rev(h))) }
  }

  /** Converter for types `FunctionN` where the first element is a `Request` to an `HList` */
  implicit def instance1[F[_], T <: HList, TR <: HList, FU, R](implicit F: Monad[F], w: EntityEncoder[F, R], fp: FnToProduct.Aux[FU, Request[F] :: TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[F, R]]): HListToFunc[F, T, FU] = new MatcherHListToFunc[F, T, FU] {
    override def matcher: ResultMatcher[F, R] = m.value
    override def conv(f: FU): (Request[F], T) => F[Response[F]] = (req: Request[F], h: T) => { matcher.conv(req, f.toProduct(req :: rev(h))) }
  }

  // for convenience
  private trait MatcherHListToFunc[F[_], T <: HList, -FU] extends HListToFunc[F, T, FU] {
    protected def matcher: ResultMatcher[F, _]
    protected def conv(f: FU): (Request[F], T) => F[Response[F]]
    final override def toAction(f: FU) = Action(matcher.resultInfo, matcher.encodings, conv(f))
  }
}
