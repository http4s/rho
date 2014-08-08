package org.http4s.rho.swaggerdemo

import org.http4s.Header
import org.http4s.server.blaze.BlazeServer
import org.http4s.rho.RhoService
import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.json4s.jackson.Json4sJacksonSupport
import org.json4s.JValue
import org.json4s.JsonDSL._

object MyService extends RhoService with SwaggerSupport with Json4sJacksonSupport {
  import org.http4s.rho._

  GET / "hello" |>> { () => "Hello world!" }
  GET / "hello" / pathVar[Int] |>> { i: Int => s"You returned $i" }
  GET / "result" / pathVar[String] +? param[Int]("id") |>> { (name: String, id: Int) => (name -> id): JValue }
}

object Main extends App {
  val builder = BlazeServer.newBuilder
  builder.mountService(MyService.andThen(_.addHeader(Header.`Access-Control-Allow-Origin`.name, "*")))
    .withPort(8080)
    .build
    .run()
}
