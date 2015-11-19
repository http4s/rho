package org.http4s
package rho

import org.http4s.rho.bits.PathAST.{TypedPath, PathRule}
import org.http4s.rho.bits.RequestRuleAST.RequestRule
import org.http4s.rho.bits.ResultInfo

import shapeless.{HNil, HList}

import scalaz.concurrent.Task

/** A shortcut type to bundle everything needed to define a route */
final case class RhoRoute[T <: HList](router: RoutingEntity[T], action: Action[T])
      extends RoutePrependable[RhoRoute[T]]
{

  def apply(req: Request, hlist: T): Task[Response] = action.act(req, hlist)

  def /:(prefix: TypedPath[HNil]): RhoRoute[T] = {
    copy(router = prefix /: router)
  }

  def method: Method = router.method
  def path: PathRule = router.path
  def requestRules: RequestRule = router.requestRules
  def responseEncodings: Set[MediaType] = action.responseEncodings
  def resultInfo: Set[ResultInfo] = action.resultInfo
  def validMedia: Set[MediaRange] = router match {
    case r: CodecRouter[_,_] => r.decoder.consumes
    case _ => Set.empty
  }
}

object RhoRoute {
  type Tpe = RhoRoute[_ <: HList]
}
