package org.http4s
package rho
package bits

import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class HListToFuncSpec extends Specification {

  def getBody(b: EntityBody): String = {
    new String(b.runLog.unsafeRun.foldLeft(ByteVector.empty)(_ :+ _).toArray)
  }

  def checkOk(r: Request): String = getBody(service(r).unsafeRun.orNotFound.body)

  def Get(s: String, h: Header*): Request =
    Request(bits.MethodAliases.GET, Uri.fromString(s).right.getOrElse(sys.error("Failed.")), headers = Headers(h:_*))

  val service = new RhoService {
    GET / "route1" |>> { () => Ok("foo") }
  }.toService()

  "HListToFunc" should {
    "Work for methods of type _ => Task[Response]" in {
      val req = Get("/route1")
      checkOk(req) should_== "foo"
    }

  }
}
