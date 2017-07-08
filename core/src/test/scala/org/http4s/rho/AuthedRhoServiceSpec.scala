package org.http4s
package rho

import java.util.UUID
import org.specs2.mutable.Specification
import fs2.{Stream, Task}
import scodec.bits.ByteVector
import cats.data._
import org.http4s.server.AuthMiddleware


case class User(name: String, id: UUID)

object Auth {

  val authUser: Service[Request, User] = Kleisli({ _ =>
    Task.now(User("Test User", UUID.randomUUID()))
  })

  val authenticated = AuthMiddleware(authUser)
}


class AuthedRhoServiceSpec extends Specification {

  implicit val userAuthInfo = new AuthInfo[User] {
    override def serialize(u: User): String = s"${u.name},${u.id}"

    override def fromString(sv: String): User = {
      val params = sv.split(",")
      User(params(0), UUID.fromString(params(1)))
    }
  }

  val service = Auth.authenticated(new AuthedRhoService[User] {
    GET +? param("foo", "bar") |>> { (req: Request, foo: String) =>
      withAuth(req) { user =>
        if (user.name == "Test User") {
          Ok(s"just root with parameter 'foo=$foo'")
        } else {
          BadRequest("This should not have happened.")
        }
      }
    }
  }.toService())

  "AuthedRhoService execution" should {

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
