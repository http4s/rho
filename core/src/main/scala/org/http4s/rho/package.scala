package org.http4s

import cats._
import cats.data.Kleisli
import scala.collection.immutable.Seq
import org.http4s.rho.{PathBuilder, PathEmpty, ResultSyntaxInstances, RhoDslHeaderExtractors, RhoDslPathExtractors, RhoDslQueryParamExtractors}
import org.http4s.rho.bits._
import org.http4s.rho.bits.PathAST._
import shapeless.{HList, HNil}

package object rho extends org.http4s.syntax.AllSyntax {
  type RhoMiddleware[F[_]] = Seq[RhoRoute[F, _ <: HList]] => Seq[RhoRoute[F, _ <: HList]]
  type AuthedRhoRoutes[F[_], U] = RhoRoutes[Kleisli[F, U, ?]]

  implicit def authedRhoRoutesSyntax[F[_], U](rhoRoutes: RhoRoutes[Kleisli[F, U, ?]]): AuthedRhoRoutesSyntax[F,U] =
    AuthedRhoRoutesSyntax[F, U](rhoRoutes)

  implicit def authedRhoRoutesRawSyntax[F[_]: Monad: Defer, U](rhoRoutes: Seq[RhoRoute[Kleisli[F, U, ?], _ <: HList]]): AuthedRhoRoutesSyntax[F,U] =
    AuthedRhoRoutesSyntax[F, U](new RhoRoutes(rhoRoutes))

  val PathEmpty: PathRule = PathMatch("")
}

trait RhoDsl[F[_]]
  extends RhoDslQueryParamExtractors[F]
    with RhoDslPathExtractors[F]
    with RhoDslHeaderExtractors[F]
    with ResultSyntaxInstances[F]
    with QueryParsers[F]
    with MatchersHListToFunc[F]
    with FuncParamsMatchers[F]
    with ResponseGeneratorInstances[F]
    with FailureResponseOps[F]
    with ResultMatchers[F] {

  implicit def method(m: Method): PathBuilder[F, HNil] = new PathBuilder(m, PathEmpty)

  /**
    * Helper to be able to define a path with one level only.
    * {{{
    * val hello = Root / "hello"
    * }}}
    */
  def root(): TypedPath[F, HNil] = TypedPath(PathEmpty)

  def * : CaptureTail.type = CaptureTail
}

object RhoDsl {
  def apply[F[_]]: RhoDsl[F] = new RhoDsl[F] {}
}