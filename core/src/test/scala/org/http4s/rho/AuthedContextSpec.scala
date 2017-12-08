package org.http4s
package rho

import java.util.UUID

import cats.data._
import cats.effect.IO
import org.http4s.server.AuthMiddleware
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

case class User(name: String, id: UUID)

object Auth {

  val authUser: Kleisli[IO, Request[IO], User] = Kleisli({ _ =>
    IO.pure(User("Test User", UUID.randomUUID()))
  })

  val authenticated = AuthMiddleware(authUser)
}


object MyAuth extends AuthedContext[User]

object MyService extends RhoService {
  import MyAuth._

  GET +? param("foo", "bar") >>> auth |>> { (req: Request[IO], foo: String, user: User) =>
    if (user.name == "Test User") {
      Ok[IO](s"just root with parameter 'foo=$foo'")
    } else {
      BadRequest[IO]("This should not have happened.")
    }
  }
}

class AuthedContextSpec extends Specification {

  val service = Auth.authenticated(MyAuth.toService(MyService.toService()))

  "AuthedContext execution" should {

    "Be able to have access to authInfo" in {
      val request = Request(Method.GET, Uri(path = "/"))
      val resp = service.run(request).unsafeRun().orNotFound
      if (resp.status == Status.Ok) {
        val body = new String(resp.body.runLog.unsafeRun.foldLeft(ByteVector.empty)(_ :+ _).toArray)
        body should_== "just root with parameter 'foo=bar'"
      } else sys.error(s"Invalid response code: ${resp.status}")
    }
  }
}
