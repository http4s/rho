package org.http4s
package rho
package bits

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite

class HListToFuncSuite extends FunSuite {
  private def getBody(b: EntityBody[IO]): String =
    new String(b.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))

  private def checkOk(r: Request[IO]): String = getBody(
    service(r).value.unsafeRunSync().getOrElse(Response.notFound).body
  )

  private def Get(s: String, h: Header.ToRaw*): Request[IO] =
    Request(
      bits.MethodAliases.GET,
      Uri.fromString(s).getOrElse(sys.error("Failed.")),
      headers = Headers(h: _*)
    )

  private val service = new RhoRoutes[IO] {
    GET / "route1" |>> { () => Ok("foo") }
  }.toRoutes()

  test("An HListToFunc should work for methods of type _ => Task[Response]") {
    val req = Get("/route1")
    assertEquals(checkOk(req), "foo")
  }

  // Tests issue 218 https://github.com/http4s/rho/issues/218
  test("An HListToFunc should work with cats.implicits") {
    import cats.implicits._
    new RhoRoutes[IO] {
      // `.pure[IO]` used to require the cats.implicits under test
      GET / "route1" |>> { () => "foo".pure[IO].flatMap(Ok(_)) }
    }
  }
}
