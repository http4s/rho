package org.http4s
package rho.bits

import org.http4s.rho.Action
import shapeless.{HNil, ::, HList}
import scalaz.concurrent.Task

/////////////////// Helpers for turning a function of may params to a function of a HList
// The library https://github.com/sbt/sbt-boilerplate may be useful for final generation

/**
 * Maps an F to the HList T
 * @tparam T HList type of the incoming values
 * @tparam F type of element onto which T will be mapped
 */
trait HListToFunc[T <: HList, -F] {
  def toAction(f: F): Action[T]
}

// for convenience
private trait MatcherHListToFunc[T <: HList, -F] extends HListToFunc[T, F] {
  protected def matcher: ResultMatcher[_]
  protected def conv(f: F): (Request, T) => Task[Response]
  final override def toAction(f: F) = Action(matcher.resultInfo, matcher.encodings, conv(f))
}

object HListToFunc {
  import shapeless._
  import shapeless.ops.hlist._
  import shapeless.ops.function._
  import shapeless.syntax.std.function._
  
  implicit def const0[R](implicit m: ResultMatcher[R]): HListToFunc[HNil, R] = new MatcherHListToFunc[HNil,R] {
    override def matcher = m
    override def conv(r: R): (Request, HNil) => Task[Response] = (req, _) => m.conv(req, r)
  }

  implicit def instance[T <: HList, TR <: HList, F, R](implicit fp: FnToProduct.Aux[F, TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[R]]): HListToFunc[T, F] = new MatcherHListToFunc[T, F] {
    override val matcher = m.value
    override def conv(f: F): (Request, T) => Task[Response] = (req: Request, h: T) => { matcher.conv(req, f.toProduct(rev(h))) }
  }  
  
  implicit def instance1[T <: HList, TR <: HList, F, R](implicit fp: FnToProduct.Aux[F, Request :: TR => R], rev: Reverse.Aux[T, TR], m: Lazy[ResultMatcher[R]]): HListToFunc[T, F] = new MatcherHListToFunc[T, F] {
    override val matcher = m.value
    override def conv(f: F): (Request, T) => Task[Response] = (req: Request, h: T) => { matcher.conv(req, f.toProduct(req :: rev(h))) }
  }
}
