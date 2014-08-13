package com.http4s.rho.swaggerdemo

import java.nio.charset.StandardCharsets

import org.http4s.Http4sConstants._
import org.http4s.{ Headers, Header }
import org.http4s.Header.{ `Content-Type`, `Access-Control-Allow-Origin` }
import org.http4s.Writable.Entity
import org.http4s.server.blaze.BlazeServer
import org.http4s.rho.RhoService
import org.http4s.rho.swagger.SwaggerSupport

case class JsonResult(name: String, number: Int)

object MyService extends RhoService with SwaggerSupport {
  import org.http4s.rho._
  import JsonWritable.jsonWritable

  GET / "hello" |>> { () => "Hello world!" }
  GET / "hello" / pathVar[Int] |>> { i: Int => s"You returned $i" }
  GET / "result" / pathVar[String] +? param[Int]("id") |>> { (name: String, id: Int) => JsonResult(name, id) }
}

object JsonWritable {
  import org.json4s._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.write
  import scalaz.stream.Process.emit
  import scalaz.concurrent.Task
  import scodec.bits.ByteVector
  import org.http4s.Writable

  private implicit val formats = Serialization.formats(NoTypeHints)

  implicit def jsonWritable[A <: AnyRef with Product]: Writable[A] =
    Writable[A](a => Task.now {
      val bytes = write(a).getBytes(StandardCharsets.UTF_8)
      Entity(emit(ByteVector.view(bytes)), Some(bytes.length))
    }, Headers.empty).withContentType(`Content-Type`.`application/json`)
}

object Main extends App {
  val builder = BlazeServer.newBuilder
  val service = MyService.andThen { t =>
    for (r <- t) yield r.addHeaders(Header(`Access-Control-Allow-Origin`.name.toString, "*"))
  }
  builder.mountService(service)
    .withPort(8080)
    .build
    .run()
}
