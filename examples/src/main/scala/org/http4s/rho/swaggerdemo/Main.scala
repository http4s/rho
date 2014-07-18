package org.http4s.rho.swaggerdemo

import org.http4s.blaze.BlazeServer
import org.http4s.rho.RhoService
import org.http4s.rho.swagger.SwaggerSupport

object MyService extends RhoService with SwaggerSupport {
  import org.http4s.rho._

  GET / "hello" |>> { () => "Hello world!" }
  GET / "hello" / pathVar[Int] |>> { i: Int => s"You returned $i" }
}

object Main {

  def main(args: Array[String]) {
    println("Hello world!")

    val builder = BlazeServer.newBuilder
    builder.mountService(MyService)
           .withPort(8080)
           .build
           .run()

  }

}
