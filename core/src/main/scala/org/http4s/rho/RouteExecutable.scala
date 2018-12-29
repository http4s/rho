package org.http4s
package rho

import org.http4s.rho.bits.HListToFunc
import shapeless.HList

/** Object which can be compiled into a complete route
  * The [[RouteExecutable]] is a complete set of HTTP method, path, query, and headers
  * needed for a compiler to generate a complete route description.
  * @tparam T The `HList` representation of the types the route expects to extract
  *           from a `Request`.
  */
case class RouteExecutableSyntax[F[_], T <: HList, RE](builder: RE)
                                                      (implicit re: RouteExecutable[F, T, RE]) {

  /** Compiles a HTTP request definition into an action */
  final def |>>[U, R, RR[G[_], T0 <: HList] <: AbstractRhoRoute[G, T0]](f: U)(implicit
                            srvc: CompileRoutes.Aux[F, RR, R],
                            routeBuilder: RouteBuilder.Aux[F, RR],
                            hltf: HListToFunc[F, RR[F, T]#RequestType, T, U]
                            ): R = {
    val action: AbstractAction[F, RR[F, T]#RequestType, T] = hltf.toAction(f)
    val entity: RoutingEntity[F, T] = re.routingEntity(builder)
    val route: RR[F, T] = routeBuilder.makeRoute[T](entity, action)
    srvc.compile(route)
  }
}

trait RouteExecutable[F[_], T <: HList, RE] {
  def routingEntity(re: RE): RoutingEntity[F, T]
}


sealed trait RouteBuilder[F[_]] {
  type RouteType[G[_], T0 <: HList] <: AbstractRhoRoute[G, T0]

  /** Create a [[RhoRoute]] from a given [[Action]] */
  def makeRoute[T <: HList](router: RoutingEntity[F, T], action: AbstractAction[F, RouteType[F, T]#RequestType, T]): RouteType[F, T]
}
object RouteBuilder {
  type Aux[F[_], RR[G[_], T <: HList] <: AbstractRhoRoute[G, T]] =
    RouteBuilder[F] { type RouteType[G[_], T0 <: HList] = RR[G, T0] }

  implicit def unauthedRhoRoute[F[_]]: RouteBuilder.Aux[F, UnAuthedRhoRoute] =
    new RouteBuilder[F] {
      override type RouteType[G[_], T0 <: HList] = UnAuthedRhoRoute[G, T0]
      override def makeRoute[T <: HList](router: RoutingEntity[F, T],
                                         action: Action[F, T]): UnAuthedRhoRoute[F, T] =
        UnAuthedRhoRoute(router, action)
    }

  implicit def preAuthedRhoRoute[F[_], U]: RouteBuilder.Aux[F, λ[(G[_], `T <: HList`) => PreAuthedRhoRoute[G, U, T]]] =
    new RouteBuilder[F] {
      override type RouteType[G[_], T0 <: HList] = PreAuthedRhoRoute[G, U, T0]
      override def makeRoute[T <: HList](router: RoutingEntity[F, T],
                                         action: AuthedAction[F, T, U]): PreAuthedRhoRoute[F, U, T] =
        PreAuthedRhoRoute(router, action)
    }
}
