package org.http4s
package rho

import java.util.UUID

import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import org.http4s.server.AuthMiddleware
import org.specs2.mutable.Specification

case class User(name: String, id: UUID)

object Auth {
  type O[A] = OptionT[IO, A]

  val authUser: Kleisli[O, Request[IO], User] = Kleisli({ _ =>
    OptionT.some[IO](User("Test User", UUID.randomUUID()))
  })

  val authenticated = AuthMiddleware(authUser)
}


object MyAuth extends AuthedContext[IO, User]

object MyRoutes extends RhoRoutes[IO] {
  import MyAuth._

  GET +? param("foo", "bar") >>> auth |>> { (foo: String, user: User) =>
    if (user.name == "Test User") {
      Ok(s"just root with parameter 'foo=$foo'")
    } else {
      BadRequest("This should not have happened.")
    }
  }

  GET / "public" / 'place |>> { path: String => Ok(s"not authenticated at $path") }

  GET / "private" / 'place |>> { (req: Request[IO], path: String) =>
    getAuth(req) match {
      case Some(user) => Ok(s"${user.name} at $path")
      case None => Forbidden(s"not authenticated at $path")
    }
  }
}

class AuthedContextSpec extends Specification {

  val routes = Auth.authenticated(MyAuth.toService(MyRoutes.toRoutes()))

  "AuthedContext execution" should {

    "Be able to have access to authInfo" in {
      val request = Request[IO](Method.GET, Uri(path = "/"))
      val resp = routes.run(request).value.unsafeRunSync().getOrElse(Response.notFound)
      if (resp.status == Status.Ok) {
        val body = new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
        body should_== "just root with parameter 'foo=bar'"
      } else ko(s"Invalid response code: ${resp.status}")
    }

    "Does not prevent route from being executed without authentication" in {
      val request = Request[IO](Method.GET, Uri(path = "/public/public"))
      val resp = routes.run(request).value.unsafeRunSync().getOrElse(Response.notFound)
      if (resp.status == Status.Ok) {
        val body = new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
        body should_== "not authenticated at public"
      } else ko(s"Invalid response code: ${resp.status}")
    }

    "Does not prevent route from being executed without authentication, but allows to extract it" in {
      val request = Request[IO](Method.GET, Uri(path = "/private/private"))
      val resp = routes.run(request).value.unsafeRunSync().getOrElse(Response.notFound)
      if (resp.status == Status.Ok) {
        val body = new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
        body should_== "Test User at private"
      } else ko(s"Invalid response code: ${resp.status}")
    }
  }
}
