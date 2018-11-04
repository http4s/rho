package org.http4s.rho

import cats.data.Kleisli
import cats.{Functor, ~>}
import org.http4s.{AuthedRequest, HttpRoutes}
import org.http4s.AuthedRoutes

trait AuthedRhoRoutesOps {
  def toAuthedService[F[_]: Functor, U](authedRhoRoutes: AuthedRhoRoutes[F, U]): AuthedRoutes[U, F] = {
    val routes: HttpRoutes[Kleisli[F, U, ?]] = authedRhoRoutes.toRoutes()
    val toAuth: F ~> Kleisli[F, U, ?] = new ~>[F, Kleisli[F, U, ?]] {
      override def apply[A](fa: F[A]): Kleisli[F, U, A] = Kleisli.liftF(fa)
    }

    Kleisli {
      (authedRequest: AuthedRequest[F, U]) =>
        val fromAuth: Kleisli[F, U, ?] ~> F = new ~>[Kleisli[F, U, ?], F] {
          override def apply[A](fa: Kleisli[F, U, A]): F[A] = fa(authedRequest.context)
        }

        routes(authedRequest.req.mapK(toAuth)).mapK(fromAuth).map(_.mapK(fromAuth))
    }
  }
}

object AuthedRhoRoutesOps extends AuthedRhoRoutesOps

case class AuthedRhoRoutesSyntax[F[_], U](
    authedRhoRoutes: AuthedRhoRoutes[F, U]) extends AnyVal {
  def toAuthedService()(implicit ev: Functor[F]): AuthedRoutes[U, F] =
    AuthedRhoRoutesOps.toAuthedService[F, U](authedRhoRoutes)
}