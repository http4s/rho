package org.http4s
package rho
package swagger

import org.specs2.mutable.Specification

import org.http4s.rho.bits.MethodAliases.GET

class SwaggerSupportSpec extends Specification {

  import org.json4s.JsonAST._
  import org.json4s.jackson._



  val baseService = new RhoService {
    GET / "hello" |>> { () => Ok("hello world") }
    GET / "hello"/ pathVar[String] |>> { world: String => Ok("hello " + world) }
  }


  "SwaggerSupport" should {
    "Expose an API listing" in {
      val service = baseService.toService(SwaggerSupport())

      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) =
        parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/swagger.json", "/hello", "/hello/{string}")
    }

    "Support prefixed routes" in {
      val service = ("foo" /: baseService).toService(SwaggerSupport())
      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) =
        parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/swagger.json", "/foo/hello", "/foo/hello/{string}")
    }
  }
}
