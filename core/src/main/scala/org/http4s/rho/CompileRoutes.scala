package org.http4s
package rho

import cats.Monad
import cats.data.{Kleisli, OptionT}
import cats.implicits._
import org.http4s.rho.bits.{PathTree, RequestLike}
import shapeless.HList
import org.http4s.server.AuthMiddleware

/** Transforms a [[RhoRoute]] into an `RouteType`.
  *
  * This can be a stateful operation, storing the action for later execution
  * or any other type of compilation phase.
  */
trait CompileRoutes[F[_], OutRouteType] {
  type InRouteType[G[_], T <: HList]

  /** Transform the [[RhoRoute]] into a `RouteType` possibly mutating this compilers state.
    *
    * @param route [[RhoRoute]] to compile.
    * @tparam T `HList` representation of the result of the route
    * @return The result of the compilation process.
    */
  def compile[T <: HList](route: InRouteType[F, T]): OutRouteType
}

object CompileRoutes {

  type Aux[F[_], InRouteType0[_[_], _ <: HList], OutRouteType] = CompileRoutes[F, OutRouteType] { type InRouteType[G[_], T <: HList] = InRouteType0[G, T] }

  /** [[CompileRoutes]] that simply returns its argument */
  def identityCompiler[F[_]]: CompileRoutes.Aux[F, RhoRoute, RhoRoute.Tpe[F]] =
    new CompileRoutes[F, RhoRoute.Tpe[F]] {
      override type InRouteType[G[_], T <: HList] = RhoRoute[G, T]

      def compile[T <: HList](route: RhoRoute[F, T]): RhoRoute[F, T] = route
    }

  /** Importable implicit identity compiler */
  object Implicit {
    implicit def compiler[F[_]]: CompileRoutes.Aux[F, RhoRoute, RhoRoute.Tpe[F]] = identityCompiler[F]
  }
}

trait CompileHttpRoutes[F[_], RR[_[_], _ <: HList]] {
  type Rq[_[_]]
  type Out = Kleisli[OptionT[F, ?], Rq[F], Response[F]]

  /** Convert the `Seq` of [[RhoRoute]]'s like class into a [[HttpRoutes]] like class
    *
    * @param routes `Seq` of routes to bundle into a service.
    * @return An `HttpRoutes`
    */
  def foldRoutes(routes: Seq[RR[F, _ <: HList]]): Out
}

object CompileHttpRoutes {
  type Aux[F[_], RR[_[_], _ <: HList], Rq0[_[_]]] = CompileHttpRoutes[F, RR] { type Rq[G[_]] = Rq0[G] }

  def action[F[_]: Monad]: CompileHttpRoutes.Aux[F, RhoRoute, Request] =
    new CompileHttpRoutes[F, RhoRoute] {
      type Rq[G[_]] = Request[G]
      override def foldRoutes(routes: Seq[RhoRoute[F, _ <: HList]]): HttpRoutes[F] = {
        /* Since we can merge AuthedRhoRoutes with UnAuthedRhoRoutes
          we group the routes together by groups with the same middleware
          maintaining the order in the seq.
        */
        val grouped: Seq[(Option[AuthMiddleware[F, _]], List[RhoRoute[F, _ <: HList]])] =
          routes
          .foldLeft(List.empty[(Option[AuthMiddleware[F, _]], List[RhoRoute[F, _<: HList]])]) {
            (b, r) =>
              (b, r) match {
                case ((Some(am), rrs) :: t, rr@AuthedRhoRoute(am2, _, _)) if am == am2 =>
                  (Option(am) -> (rr :: rrs)) :: t
                case (t, rr@AuthedRhoRoute(am, _, _)) =>
                  (Option(am) -> (rr :: Nil)) :: t
                case ((None, rrs) :: t, rr@UnAuthedRhoRoute(_, _)) =>
                  (Option.empty -> (rr :: rrs)) :: t
                case (t, rr@UnAuthedRhoRoute(_, _)) =>
                  (Option.empty -> (rr :: Nil)) :: t
              }
          }
        /* Now we'll build each group into a HttpRoutes and compose those routes
          together maintaining the order.  This way the AuthMiddlware get reused
          at least for neighboring rho routes using the same middleware.
          If a user does AuthedRhoRoutes and UnAuthedRhoRoutes and AuthedRhoRhoutes
          using the same middleware, this will unfortunately call that middleware
          multiple times, assuming they are not using the fallthrough version
         */
        grouped
          .map {
            case (None, routes) =>
              routes.asInstanceOf[List[UnAuthedRhoRoute[F, _ <: HList]]]
                .foldLeft(PathTree[F]()){ (t, r) => t.appendRoute(r) }
                .toKleisli
            case (Some(am), routes) =>
              am.asInstanceOf[AuthMiddleware[F, Any]](
                routes.asInstanceOf[List[AuthedRhoRoute[F, Any, _ <: HList]]]
                  .foldLeft(PathTree.authed[F, Any]()(RequestLike.authedRequest[Any])){
                    (t, r) => t.appendRoute(r)
                  }
                  .toKleisli
              )
          }
          .reduce(_ <+> _)
      }
    }

  implicit def unauthedAction[F[_]: Monad]: CompileHttpRoutes.Aux[F, UnAuthedRhoRoute, Request] =
    new CompileHttpRoutes[F, UnAuthedRhoRoute] {
      type Rq[G[_]] = Request[G]
      override def foldRoutes(routes: Seq[UnAuthedRhoRoute[F, _ <: HList]]): HttpRoutes[F] = {
        routes.foldLeft(PathTree[F]()) {
            (t, r) => t.appendRoute(r)
          }
          .toKleisli
      }
    }

  implicit def authedAction[F[_]: Monad, U]: CompileHttpRoutes.Aux[F, Lambda[(G[_], `T <: HList`) => PreAuthedRhoRoute[G, U, T]], AuthedRequest[?[_], U]] =
    new CompileHttpRoutes[F, Lambda[(G[_], `T <: HList`) => PreAuthedRhoRoute[G, U, T]]] {
      type Rq[G[_]] = AuthedRequest[G, U]
      override def foldRoutes(routes: Seq[PreAuthedRhoRoute[F, U, _ <: HList]]): AuthedService[U, F] = /*{
        val tree = routes.foldLeft(PathTree[F]()){ (t, r) => t.appendRoute(r) }
        tree.toKleisli
      }*/ ???
    }
}