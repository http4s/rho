package org.http4s
package rho
package swagger

import org.specs2.mutable.Specification

import org.http4s.rho.bits.MethodAliases.GET

class SwaggerSupportSpec extends Specification with RequestRunner {

  import org.json4s.JsonAST._
  import org.json4s.jackson._

  lazy val service = new SwaggerSupport {
    GET / "hello" |>> { () => Ok("hello world") }
    GET / "hello"/ pathVar[String] |>> { world: String => Ok("hello " + world) }
  }

  "SwaggerSupport" should {

    "Expose an API listing" in {
      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) =
        parseJson(checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/swagger.json", "/hello", "/hello/{string}")
    }

    "Expose an API description" in {
      //val r = Request(GET, Uri(path = "/swagger.json/hello"))
      //val json = parseJson(checkOk(r))

      //val JArray(List(a, b)) = json \ "apis"

      //(a \ "path" should_== JString("/hello")) &&
      //(b \ "path" should_== JString("/hello/{string}"))
      pending
     }

    "Return a 404 on bad path" in {
      //val r = Request(GET, Uri(path = "/swagger.json/notfound"))
      //checkStatus(r)(_ == Status.NotFound) should_== "Api Not Found: swagger.json/notfound"
      pending
    }
  }
}
