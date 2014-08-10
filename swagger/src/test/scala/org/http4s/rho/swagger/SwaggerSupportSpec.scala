package org.http4s
package rho
package swagger

import org.specs2.mutable.Specification


class SwaggerSupportSpec extends Specification with RequestRunner {

  import org.json4s.JsonAST._
  import org.json4s.jackson._

  lazy val service = new SwaggerSupport {
    GET / "hello" |>> { () => OK("hello world") }
    GET / "hello"/ pathVar[String] |>> { world: String => OK("hello " + world) }
  }

  "SwaggerSupport" should {
    "Expose an API listing" in {
      val r = Request(GET, Uri(path = "/api-info"))
      val JObject(List((_, JString(a)), (_, JString(b)))) = parseJson(checkOk(r)) \\ "path"
      Set(a, b) should_== Set("/api-info", "/hello")
    }

    "Expose an API description" in {
      val r = Request(GET, Uri(path = "/api-info/hello"))
      val json = parseJson(checkOk(r))

//      println(checkOk(r))
      val JArray(List(a, b)) = json \ "apis"

      (a \ "path" should_== JString("/hello")) &&
      (b \ "path" should_== JString("/hello/{string}"))
    }
  }
  

}
