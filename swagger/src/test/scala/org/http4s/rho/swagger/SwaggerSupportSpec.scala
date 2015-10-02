package org.http4s
package rho
package swagger

import org.specs2.mutable.Specification

import org.http4s.rho.bits.MethodAliases.GET

class SwaggerSupportSpec extends Specification with RequestRunner {

  import org.json4s.JsonAST._
  import org.json4s.jackson._

  val service = new RhoService {
    GET / "hello" |>> { () => Ok("hello world") }
    GET / "hello"/ pathVar[String] |>> { world: String => Ok("hello " + world) }
  }.toService(SwaggerSupport().middleware)

  "SwaggerSupport" should {

    "Expose an API listing" in {
      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) =
        parseJson(checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/swagger.json", "/hello", "/hello/{string}")
    }
  }
}
