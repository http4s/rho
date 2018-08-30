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

object MyService extends RhoService[IO] {
  import MyAuth._

  GET +? param("foo", "bar") >>> auth |>> { (req: Request[IO], foo: String, user: User) =>
    if (user.name == "Test User") {
      Ok(s"just root with parameter 'foo=$foo'")
    } else {
      BadRequest("This should not have happened.")
    }
  }
}

class AuthedContextSpec extends Specification {

  val service = Auth.authenticated(MyAuth.toService(MyService.toRoutes()))

  "AuthedContext execution" should {

    "Be able to have access to authInfo" in {
      val request = Request[IO](Method.GET, Uri(path = "/"))
      val resp = service.run(request).value.unsafeRunSync().getOrElse(Response.notFound)
      if (resp.status == Status.Ok) {
        val body = new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
        body should_== "just root with parameter 'foo=bar'"
      } else sys.error(s"Invalid response code: ${resp.status}")
    }
  }
}
