package org.http4s
package rho
package bits

import cats.effect.IO
import org.specs2.mutable.Specification

class HListToFuncSpec extends Specification {
  def getBody(b: EntityBody[IO]): String =
    new String(b.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))

  def checkOk(r: Request[IO]): String = getBody(
    service(r).value.unsafeRunSync().getOrElse(Response.notFound).body
  )

  def Get(s: String, h: Header.ToRaw*): Request[IO] =
    Request(
      bits.MethodAliases.GET,
      Uri.fromString(s).getOrElse(sys.error("Failed.")),
      headers = Headers(h: _*)
    )

  val service = new RhoRoutes[IO] {
    GET / "route1" |>> { () => Ok("foo") }
  }.toRoutes()

  "HListToFunc" should {
    "Work for methods of type _ => Task[Response]" in {
      val req = Get("/route1")
      checkOk(req) should_== "foo"
    }

    // Tests issue 218 https://github.com/http4s/rho/issues/218
    "Work with cats.implicits" in {
      import cats.implicits._
      new RhoRoutes[IO] {
        // `.pure[IO]` used to require the cats.implicits under test
        GET / "route1" |>> { () => "foo".pure[IO].flatMap(Ok(_)) }
      }
      success
    }
  }
}
