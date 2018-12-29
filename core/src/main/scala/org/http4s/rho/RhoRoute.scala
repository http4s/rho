package org.http4s
package rho

import cats._
import cats.data.{Kleisli, OptionT}
import org.http4s.rho.bits.PathAST.{PathRule, TypedPath}
import org.http4s.rho.bits.RequestAST.RequestRule
import org.http4s.rho.bits.ResultInfo
import org.http4s.server.AuthMiddleware
import shapeless.{HList, HNil}

object AbstractRhoRoute {
  /** Existentially typed [[RhoRoute]] useful when the parameters are not needed */
  type Tpe[F[_]] = AbstractRhoRoute[F, _ <: HList]

  //def unapply[F[_], T <: HList](arg: AbstractRhoRoute[F,T]): Option[(RoutingEntity[F, T], AbstractAction[F, R, T] forSome {type R[_]})] = Some(arg.router, arg.action)

  type Aux[F[_], Rq[_[_]], T <: HList] = AbstractRhoRoute[F, T] { type RequestType[G[_]] = Rq[G] }
}
sealed trait AbstractRhoRoute[F[_], T <: HList] {
  type RequestType[_[_]]

  def router: RoutingEntity[F, T]
  def action: AbstractAction[F, RequestType, T]

  def responseEncodings: Set[MediaType]
  def resultInfo: Set[ResultInfo]
  def method: Method = router.method
  def path: PathRule = router.path
  def rules: RequestRule[F] = router.rules
  def validMedia: Set[MediaRange] = router match {
    case r: CodecRouter[F,_,_] => r.decoder.consumes
    case _ => Set.empty
  }
}

object RhoRoute {
  /** Existentially typed [[RhoRoute]] useful when the parameters are not needed */
  type Tpe[F[_]] = RhoRoute[F, _ <: HList]

  //def unapply[F[_], T <: HList](arg: RhoRoute[F,T]): Option[(RoutingEntity[F, T], AbstractAction[F, R, T] forSome {type R[_]})] = Some(arg.router, arg.action)
}

sealed trait RhoRoute[F[_], T <: HList]
  extends AbstractRhoRoute[F, T]
  with RoutePrependable[F, RhoRoute[F, T]]

/** A type to bundle everything needed to define a route */
final case class UnAuthedRhoRoute[F[_], T <: HList](override val router: RoutingEntity[F, T],
                                                                 action: Action[F, T])
  extends RhoRoute[F, T]
{
  override type RequestType[G[_]] = Request[G]

  override def responseEncodings: Set[MediaType] = action.responseEncodings
  override def resultInfo: Set[ResultInfo] = action.resultInfo

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
  override def /:(prefix: TypedPath[F, HNil]): UnAuthedRhoRoute[F, T] = {
    copy(router = prefix /: router)
  }
}
object UnAuthedRhoRoute {
  /** Existentially typed [[UnAuthedRhoRoute]] useful when the parameters are not needed */
  type Tpe[F[_]] = UnAuthedRhoRoute[F, _ <: HList]
}

final case class AuthedRhoRoute[F[_], U, T <: HList](authMiddleware: AuthMiddleware[F, U],
                                                     override val router: RoutingEntity[F, T],
                                                                  action: AuthedAction[F, T, U])
  extends RhoRoute[F, T]
{
  override type RequestType[G[_]] = AuthedRequest[G, U]

  override def responseEncodings: Set[MediaType] = action.responseEncodings
  override def resultInfo: Set[ResultInfo] = action.resultInfo

  /** Execute the [[AuthedRhoRoute]]
    *
    * @param req The `AuthedRequest` to be served.
    * @param hlist Parameters obtained by executing the rules.
    * @return A `Response` to the `AuthedRequest`.
    */
  def apply(req: AuthedRequest[F, U], hlist: T): F[Response[F]] = action.act(req, hlist)
  /** Execute the [[AuthedRhoRoute]] using the attached authMiddleware
    *
    * @param req The `Request` to be served.
    * @param hlist Parameters obtained by executing the rules.
    * @return A `Response` to the `AuthedRequest`.
    */
  def apply(req: Request[F], hlist: T)(implicit F: Functor[F]): OptionT[F, Response[F]] =
    authMiddleware(Kleisli((ar: AuthedRequest[F, U]) => action.act(ar, hlist)).mapF(OptionT.liftF[F, Response[F]](_)))(req)

  /** Prefix the [[RhoRoute]] with non-capturing path rules
    *
    * @param prefix non-capturing prefix to prepend
    * @return builder with the prefix prepended to the path rules
    */
  override def /:(prefix: TypedPath[F, HNil]): RhoRoute[F, T] = {
    copy(router = prefix /: router)
  }
}

object PreAuthedRhoRoute {
  /** Existentially typed [[PreAuthedRhoRoute]] useful when the parameters are not needed */
  type Tpe[F[_], U] = PreAuthedRhoRoute[F, U, _ <: HList]
}

final case class PreAuthedRhoRoute[F[_], U, T <: HList](router: RoutingEntity[F, T], action: AuthedAction[F, T, U])
  extends AbstractRhoRoute[F, T]
  with RoutePrependable[F, PreAuthedRhoRoute[F, U, T]] {

  override type RequestType[G[_]] = AuthedRequest[G, U]

  override def responseEncodings: Set[MediaType] = action.responseEncodings
  override def resultInfo: Set[ResultInfo] = action.resultInfo

  /** Execute the [[PreAuthedRhoRoute]]
    *
    * @param req The `AuthedRequest` to be served.
    * @param hlist Parameters obtained by executing the rules.
    * @return A `Response` to the `AuthedRequest`.
    */
  def apply(req: AuthedRequest[F, U], hlist: T): F[Response[F]] = action.act(req, hlist)


  def attachAuthMiddleware(authMiddleware: AuthMiddleware[F, U]): AuthedRhoRoute[F, U, T] =
    AuthedRhoRoute(authMiddleware, router, action)

  /** Prefix the [[RhoRoute]] with non-capturing path rules
    *
    * @param prefix non-capturing prefix to prepend
    * @return builder with the prefix prepended to the path rules
    */
  override def /:(prefix: TypedPath[F, HNil]): PreAuthedRhoRoute[F, U, T] = {
    copy(router = prefix /: router)
  }
}