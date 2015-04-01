package com.http4s.rho.swagger.demo

import java.util.concurrent.atomic.AtomicInteger

import org.http4s.Uri
import org.http4s.UrlForm
import org.http4s.rho.RhoService
import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.scalaxml._

import JsonEncoder.AutoSerializable
import org.http4s.util.UrlFormCodec
import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

object MyService extends RhoService with SwaggerSupport {
  import org.http4s.rho._
  import org.http4s.rho.swagger._
  import org.http4s.EntityDecoder

  import org.http4s.headers
  import org.http4s.{Request, Headers, DateTime}

  case class JsonResult(name: String, number: Int) extends AutoSerializable

  val requireCookie = requireThatR(headers.Cookie){ cookie =>
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

  "Generates some JSON data from a route param, and a query Int" **
    GET / "result" / 'foo +? param[Int]("id") |>>
    { (name: String, id: Int) => Ok(JsonResult(name, id)) }

  "Two different response codes can result from this route based on the number given" **
    GET / "differentstatus" / pathVar[Int] |>> { i: Int =>
      if (i >= 0) Ok(JsonResult("Good result", i))
      else BadRequest(<html><body>Negative number: { i }</body></html>)
    }

  "This gets a simple counter for the number of times this route has been requested" **
    GET / "counter" |>> {
      val i = new AtomicInteger(0)
      Task(<html><body><h2>{ s"The number is ${i.getAndIncrement()}" }</h2></body></html>)
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
        Headers(cookie.values.toList.map { c => headers.`Set-Cookie`(c.copy(expires = Some(DateTime.UnixEpoch), maxAge = Some(0)))})
    }

    Ok("Deleted cookies!").withHeaders(hs)
  }

  "This route allows your to post stuff" **
    POST / "post" ^ EntityDecoder.text |>> { body: String =>
      "You posted: " + body
    }

  "This demonstrates using a process of entities" **
    GET / "stream" |>> {
      val s = 0 until 100 map (i => s"Hello $i\n")
      val p: Process[Task, String] = Process.emitAll(s)
      Ok(p)
    }
}
