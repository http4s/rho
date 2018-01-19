package org.http4s
package rho
package bits

import cats.effect.IO
import org.http4s.rho.io._
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class HListToFuncSpec extends Specification {
  def getBody(b: EntityBody[IO]): String = {
    new String(b.compile.toVector.unsafeRunSync().foldLeft(ByteVector.empty)(_ :+ _).toArray)
  }

  def checkOk(r: Request[IO]): String = getBody(service(r).value.unsafeRunSync().getOrElse(Response.notFound).body)

  def Get(s: String, h: Header*): Request[IO] =
    Request(bits.MethodAliases.GET, Uri.fromString(s).right.getOrElse(sys.error("Failed.")), headers = Headers(h:_*))

  val service = new RhoService[IO] {
    GET / "route1" |>> { () => Ok("foo") }
  }.toService()

  "HListToFunc" should {
    "Work for methods of type _ => Task[Response]" in {
      val req = Get("/route1")
      checkOk(req) should_== "foo"
    }
  }
}
