package com.http4s.rho.swagger.demo

import cats.Monad
import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import org.http4s.rho.AuthedRhoRoutes
import org.http4s.Request
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.server.AuthMiddleware
import org.http4s.util.CaseInsensitiveString

object ExampleAuth {
  val simpleAuthService: Kleisli[OptionT[IO, ?], Request[IO], SimpleUser] =
    Kleisli { request =>

      OptionT.fromOption[IO] {
        request.params.get("api_key").orElse(request.headers.get(CaseInsensitiveString("user")).map(_.value)).map(h => SimpleUser(h))
      }
    }
  val simpleAuthMiddlware: AuthMiddleware[IO, SimpleUser] = AuthMiddleware.apply(simpleAuthService)

}
case class SimpleUser(name: String)

class MyAuthedRoutes[F[_]: Monad]
  extends AuthedRhoRoutes[F, SimpleUser]
  with SwaggerSyntax[F] {

  "Simple route" **
    GET / "authed" / "ping" |>> Ok("pong!")

  "Simple hello world route" **
    GET / "authed" / "hello" |>> { (user: SimpleUser) =>
      Ok(s"Hello ${user.name}!")
    }
}
