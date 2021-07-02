package org.http4s
package rho

import java.util.UUID

import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import munit.FunSuite
import org.http4s.server.AuthMiddleware

case class User(name: String, id: UUID)

object Auth {
  type O[A] = OptionT[IO, A]

  val authUser = Kleisli[O, Request[IO], User] { _ =>
    OptionT.some[IO](User("Test User", UUID.randomUUID()))
  }

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

  GET / "public" / pv"place" |>> { path: String => Ok(s"not authenticated at $path") }

  GET / "private" / pv"place" |>> { (req: Request[IO], path: String) =>
    getAuth(req) match {
      case Some(user) => Ok(s"${user.name} at $path")
      case None => Forbidden(s"not authenticated at $path")
    }
  }
}

class AuthedContextSuite extends FunSuite {
  import cats.effect.unsafe.implicits.global

  val routes = Auth.authenticated(MyAuth.toService(MyRoutes.toRoutes()))

  test("An AuthedContext execution should be able to have access to authInfo") {
    val request = Request[IO](Method.GET, uri"/")
    val resp = routes.run(request).value.unsafeRunSync().getOrElse(Response.notFound)
    if (resp.status == Status.Ok) {
      val body =
        new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
      assertEquals(body, "just root with parameter 'foo=bar'")
    } else fail(s"Invalid response code: ${resp.status}")
  }

  test(
    "An AuthedContext execution does not prevent route from being executed without authentication"
  ) {
    val request = Request[IO](Method.GET, uri"/public/public")
    val resp = routes.run(request).value.unsafeRunSync().getOrElse(Response.notFound)
    if (resp.status == Status.Ok) {
      val body =
        new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
      assertEquals(body, "not authenticated at public")
    } else fail(s"Invalid response code: ${resp.status}")
  }

  test(
    "An AuthedContext execution does not prevent route from being executed without authentication, " +
      "but allows to extract it"
  ) {
    val request = Request[IO](Method.GET, uri"/private/private")
    val resp = routes.run(request).value.unsafeRunSync().getOrElse(Response.notFound)
    if (resp.status == Status.Ok) {
      val body =
        new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
      assertEquals(body, "Test User at private")
    } else fail(s"Invalid response code: ${resp.status}")
  }
}
