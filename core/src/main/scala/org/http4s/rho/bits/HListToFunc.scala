package org.http4s
package rho.bits

import cats.Monad
import org.http4s.rho.AbstractAction
import shapeless._
import shapeless.ops.function._
import shapeless.ops.hlist._
import shapeless.syntax.std.function._

/** Converter of an value of type F to the HList of type T
  *
 * @tparam T HList type of the incoming values
 * @tparam U type of element onto which T will be mapped
 */
trait HListToFunc[F[_], R[_[_]], T <: HList, -U] {
  def toAction(f: U): AbstractAction[F, R, T]
}

trait AbstractMatchersHListToFunc[F[_], RQ[_[_]]] {
  type ActionType[G[_], T0 <: HList] = AbstractAction[G, RQ, T0]

  /** Converter of any type with a result matcher when there are no values on the stack
    *
    * @tparam R type of result
    */
  implicit def const0[R](implicit F: Monad[F], rq: RequestLike[RQ],m: ResultMatcher[F, R]): HListToFunc[F, RQ, HNil, R] = new MatcherHListToFunc[HNil, R] {
    override def matcher: ResultMatcher[F, R] = m
    override def conv(r: R): (RQ[F], HNil) => F[Response[F]] = (req, _) => m.conv(req, r)
  }

  /** Converter for types `FunctionN` to an `HList` */
  implicit def instance[T <: HList, TR <: HList, FU, R](implicit F: Monad[F], rq: RequestLike[RQ], fp: FnToProduct.Aux[FU, TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[F, R]]): HListToFunc[F, RQ, T, FU] = new MatcherHListToFunc[T, FU] {
    override def matcher: ResultMatcher[F, R] = m.value
    override def conv(f: FU): (RQ[F], T) => F[Response[F]] = (req: RQ[F], h: T) => { matcher.conv(req, f.toProduct(rev(h))) }
  }

  /** Converter for types `FunctionN` where the first element is a `RQ` to an `HList` */
  implicit def instanceRQ[T <: HList, TR <: HList, FU, R](implicit F: Monad[F], rq: RequestLike[RQ], fp: FnToProduct.Aux[FU, RQ[F] :: TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[F, R]]): HListToFunc[F, RQ, T, FU] = new MatcherHListToFunc[T, FU] {
    override def matcher: ResultMatcher[F, R] = m.value
    override def conv(f: FU): (RQ[F], T) => F[Response[F]] = (req: RQ[F], h: T) => { matcher.conv(req, f.toProduct(req :: rev(h))) }
  }

  // for convenience
  private[bits] trait MatcherHListToFunc[T <: HList, -FU] extends HListToFunc[F, RQ, T, FU] {
    protected def matcher: ResultMatcher[F, _]
    protected def conv(f: FU): (RQ[F], T) => F[Response[F]]
    final override def toAction(f: FU): AbstractAction[F, RQ, T] = AbstractAction(matcher.resultInfo, matcher.encodings, conv(f))
  }
}

trait MatchersHListToFunc[F[_]] extends AbstractMatchersHListToFunc[F, Request]

trait AuthedMatchersHListToFunc[F[_], U] extends AbstractMatchersHListToFunc[F, AuthedRequest[?[_], U]] {
  type RQ[G[_]] = AuthedRequest[G, U]

  /** Converter for types `FunctionN` where the first element is a `Request` to an `HList` */
  implicit def instanceRequest[T <: HList, TR <: HList, FU, R](implicit F: Monad[F],
                                                               rq: RequestLike[RQ],
                                                               fp: FnToProduct.Aux[FU, Request[F] :: TR => R],
                                                               rev: Reverse.Aux[T, TR],
                                                               m: Lazy[ResultMatcher[F, R]]): HListToFunc[F, RQ, T, FU] = new MatcherHListToFunc[T, FU] {
    override def matcher: ResultMatcher[F, R] = m.value
    override def conv(f: FU): (RQ[F], T) => F[Response[F]] =
      (req: RQ[F], h: T) => { matcher.conv(req, f.toProduct(req.req :: rev(h))) }
  }

  /** Converter for types `FunctionN` where the first element is a `U` to an `HList` */
  implicit def instanceUser[T <: HList, TR <: HList, FU, R](implicit F: Monad[F],
                                                            rq: RequestLike[RQ],
                                                            fp: FnToProduct.Aux[FU, U :: TR => R],
                                                            rev: Reverse.Aux[T, TR],
                                                            m: Lazy[ResultMatcher[F, R]]): HListToFunc[F, RQ, T, FU] = new MatcherHListToFunc[T, FU] {
    override def matcher: ResultMatcher[F, R] = m.value
    override def conv(f: FU): (RQ[F], T) => F[Response[F]] =
      (req: RQ[F], h: T) => { matcher.conv(req, f.toProduct(req.authInfo :: rev(h))) }
  }
}