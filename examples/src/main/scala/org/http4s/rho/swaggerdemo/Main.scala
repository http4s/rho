package org.http4s.rho.swaggerdemo

import org.http4s.Header.`Content-Type`
import org.http4s.Header
import org.http4s.blaze.BlazeServer
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

object Main {
  def main(args: Array[String]) {
    println("Hello world!")

    val builder = BlazeServer.newBuilder
    builder.mountService(MyService.andThen(_.addHeader(Header.Raw(Header.`Access-Control-Allow-Origin`.name, "*"))))
           .withPort(8080)
           .build
           .run()
  }
}

object JsonWritable {
  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.{read, write}
  import scalaz.stream.Process.emit
  import scalaz.concurrent.Task
  import scodec.bits.ByteVector
  import org.http4s.{CharacterSet, HttpBody, Writable}

  private implicit val formats = Serialization.formats(NoTypeHints)

  implicit def jsonWritable[A <: AnyRef with Product]: Writable[A] = new Writable[A] {
    override def contentType = `Content-Type`.`application/json`

    override def toBody(a: A): Task[(HttpBody, Option[Int])] = {
      val bytes = write(a).getBytes(CharacterSet.`UTF-8`.charset)
      Task.now(emit(ByteVector.view(bytes)) -> Some(bytes.length))
    }
  }
}