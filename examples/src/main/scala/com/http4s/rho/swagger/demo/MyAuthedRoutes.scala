package com.http4s.rho.swagger.demo

import cats.data.{Kleisli, OptionT}
import cats.effect.{Effect, IO}
import org.http4s.Request
import org.http4s.rho._
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.server.AuthMiddleware
import org.http4s.util.CaseInsensitiveString

object ExampleAuth {
  val simpleAuthService: Kleisli[OptionT[IO, ?], Request[IO], SimpleUser] =
    Kleisli { request =>
      OptionT.fromOption[IO] {
        request.headers.get(CaseInsensitiveString("user")).map(h => SimpleUser(h.value))
      }
    }
  val simpleAuthMiddlware: AuthMiddleware[IO, SimpleUser] = AuthMiddleware.withFallThrough(simpleAuthService)

}
case class SimpleUser(name: String)

class MyAuthedRoutes[F[+_] : Effect]
  extends AuthedRhoRoutes[F, SimpleUser]
    with SwaggerSyntax[Kleisli[F, SimpleUser, ?]] {
  type AF[A] = Kleisli[F, SimpleUser, A]

  "Simple route" **
    GET / "ping" |>> Ok("pong!")

  "Simple hello world route" **
    GET / "hello" |>> { () =>
    Kleisli.ask[F, SimpleUser].flatMap { user =>
      Ok(s"Hello ${user.name}!")
    }
  }
}
