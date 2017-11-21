package org.http4s
package rho
package bits

import cats.effect.IO
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class HListToFuncSpec extends Specification {

  def getBody(b: EntityBody[IO]): String = {
    new String(b.runLogSync.unsafeRunSync.foldLeft(ByteVector.empty)(_ :+ _).toArray)
  }

  def checkOk(r: Request[IO]): String = getBody(service(r).getOrElse(Response.notFound[IO]).unsafeRunSync().body)

  def Get(s: String, h: Header*): Request[IO] =
    Request(bits.MethodAliases.GET, Uri.fromString(s).right.getOrElse(sys.error("Failed.")), headers = Headers(h:_*))

  val service = new RhoService {
    GET / "route1" |>> { () => Ok("foo") }
  }.toService()

  "HListToFunc" should {
    "Work for methods of type _ => IO[Response[IO]" in {
      val req = Get("/route1")
      checkOk(req) should_== "foo"
    }

  }
}
