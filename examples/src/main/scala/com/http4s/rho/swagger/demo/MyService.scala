package com.http4s.rho.swagger.demo

import java.util.concurrent.atomic.AtomicInteger
import java.time.Instant

import org.http4s.Uri
import org.http4s.rho.RhoService

import JsonEncoder.AutoSerializable
import cats.syntax.all._
import fs2.{Stream, Task}

object MyService extends RhoService {
  import org.http4s.rho._
  import org.http4s.rho.swagger._
  import org.http4s.EntityDecoder

  import org.http4s.headers
  import org.http4s.{Request, Headers}

  case class JsonResult(name: String, number: Int) extends AutoSerializable

  val requireCookie = existsAndR(headers.Cookie){ cookie =>
    cookie.values.toList.find(c => c.name == "Foo" && c.content == "bar") match {
      case Some(_) => None   // Cookie found, good to go.
      case None => // Didn't find cookie
        Some(TemporaryRedirect(uri("/addcookie")))
    }
  }

  "We don't want to have a real 'root' route anyway... " **
    GET |>> TemporaryRedirect(Uri(path="/swagger-ui"))

  // We want to define this chunk of the service as abstract for reuse below
  val hello = GET / "hello"

  "Simple hello world route" **
    hello |>> Ok("Hello world!")

  "A variant of the hello route that takes an Int param" **
    hello / pathVar[Int] |>> { i: Int => Ok(s"You returned $i") }

  "This route allows you to send head request" **
    HEAD / "hello" |>> { Ok("Hello head!") }

  "Generates some JSON data from a route param, and a query Int" **
    GET / "result" / 'foo +? param[Int]("id") |>> { (name: String, id: Int) => Ok(JsonResult(name, id)) }

  "Two different response codes can result from this route based on the number given" **
    GET / "differentstatus" / pathVar[Int] |>> { i: Int =>
      if (i >= 0) Ok(JsonResult("Good result", i))
      else BadRequest(s"Negative number: $i")
    }

  "This gets a simple counter for the number of times this route has been requested" **
    GET / "counter" |>> {
      val i = new AtomicInteger(0)
      Task.delay(s"The number is ${i.getAndIncrement()}")
    }

  "Adds the cookie Foo=bar to the client" **
    GET / "addcookie" |>> {
      Ok("You now have a good cookie!").addCookie("Foo", "bar")
    }

  "Sets the cookie Foo=barr to the client" **
    GET / "addbadcookie" |>> {
      Ok("You now have an evil cookie!").addCookie("Foo", "barr")
    }

  "Checks the Foo cookie to make sure its 'bar'" **
    GET / "checkcookie" >>> requireCookie |>> Ok("Good job, you have the cookie!")

  "Clears the cookies" **
    GET / "clearcookies" |>> { req: Request =>
      val hs = req.headers.get(headers.Cookie) match {
        case None => Headers.empty
        case Some(cookie) =>
          Headers(cookie.values.toList.map { c => headers.`Set-Cookie`(c.copy(expires = Some(Instant.ofEpochMilli(0)), maxAge = Some(0)))})
      }

      Ok("Deleted cookies!").replaceAllHeaders(hs)
    }

  "This route allows your to post stuff" **
    POST / "post" ^ EntityDecoder.text |>> { body: String =>
      "You posted: " + body
    }

  "This demonstrates using a process of entities" **
    GET / "stream" |>> {
      val s = 0 until 100 map (i => s"Hello $i\n")
      val p: Stream[Task, String] = Stream.emits(s)
      Ok(p)
    }

  "Get a file" **
    GET / "file" |>> Ok(SwaggerFileResponse("HELLO"))

  import scala.reflect._
  import scala.reflect.runtime.universe._

  import org.json4s._
  import org.json4s.jackson.JsonMethods

  import org.http4s.rho.bits._

  private implicit val format = DefaultFormats

  implicit def jsonParser[A : TypeTag : ClassTag]: StringParser[A] = new StringParser[A] {
    override val typeTag = implicitly[TypeTag[A]].some
    override def parse(s: String): ResultResponse[A] = {

      Either.catchNonFatal(JsonMethods.parse(s).extract[A]) match {
        case Left(t) => FailureResponse.badRequest(t.getMessage)
        case Right(t) => SuccessResponse(t)
      }
    }
  }

  case class Foo(k: String, v: Int)
  case class Bar(id: Long, foo: Foo)

  "This route demonstrates how to use a complex data type as parameters in route" **
  GET / "complex" +? param[Foo]("foo") & param[Seq[Bar]]("bar", Nil) |>> { (foo: Foo, bars: Seq[Bar]) =>
    Ok(s"Received foo: $foo, bars: $bars")
  }
}
