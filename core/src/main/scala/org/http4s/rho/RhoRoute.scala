package org.http4s
package rho

import language.existentials

import bits.HeaderAST.HeaderRule
import bits.PathAST.PathRule
import org.http4s.rho.bits.QueryAST.QueryRule
import org.http4s.rho.bits.{ResultResponse, ResultInfo}

import shapeless.HList

import scalaz.concurrent.Task

/** A shortcut type to bundle everything needed to define a route */
sealed trait RhoRoute {
  type T <: HList
  val router: RoutingEntity[T]
  val action: Action[T]

  def method: Method = router.method
  def path: PathRule = router.path
  def query: QueryRule = router.query
  def headers: HeaderRule = router.headers
  def responseEncodings: Set[MediaType] = action.responseEncodings
  def resultInfo: Set[ResultInfo] = action.resultInfo
  def validMedia: Set[MediaRange] = router.validMedia

  // This is dangerous: the pathparams are untyped and only a portion of the coproduct type T
  private[rho] def act(req: Request, pathparams: HList): ResultResponse[Task[Response]] =
    router.act(action, req, pathparams)
}

object RhoRoute {
  def apply[TT <: HList](inRouter: RoutingEntity[TT], inAction: Action[TT]): RhoRoute = new RhoRoute {
    override type T = TT
    override val action: Action[T] = inAction
    override val router: RoutingEntity[T] = inRouter
  }
}
