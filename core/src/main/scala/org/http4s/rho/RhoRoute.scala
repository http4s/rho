package org.http4s
package rho

import org.http4s.rho.bits.PathAST.{TypedPath, PathRule}
import org.http4s.rho.bits.RequestAST.RequestRule
import org.http4s.rho.bits.ResultInfo

import shapeless.{HNil, HList}

/** A type to bundle everything needed to define a route */
final case class RhoRoute[F[_], T <: HList](router: RoutingEntity[F, T], action: Action[F, T])
      extends RoutePrependable[F, RhoRoute[F, T]]
{

  /** Execute the [[RhoRoute]]
    *
    * @param req The `Request` to be served.
    * @param hlist Parameters obtained by executing the rules.
    * @return A `Response` to the `Request`.
    */
  def apply(req: Request[F], hlist: T): F[Response[F]] = action.act(req, hlist)

  /** Prefix the [[RhoRoute]] with non-capturing path rules
    *
    * @param prefix non-capturing prefix to prepend
    * @return builder with the prefix prepended to the path rules
    */
  override def /:(prefix: TypedPath[F, HNil]): RhoRoute[F, T] = {
    copy(router = prefix /: router)
  }

  def method: Method = router.method
  def path: PathRule = router.path
  def rules: RequestRule[F] = router.rules
  def responseEncodings: Set[MediaType] = action.responseEncodings
  def resultInfo: Set[ResultInfo] = action.resultInfo
  def validMedia: Set[MediaRange] = router match {
    case r: CodecRouter[F,_,_] => r.decoder.consumes
    case _ => Set.empty
  }
}

object RhoRoute {
  /** Existentially typed [[RhoRoute]] useful when the parameters are not needed */
  type Tpe[F[_]] = RhoRoute[F, _ <: HList]
}
