package com.http4s.rho.swagger.demo

import java.util.concurrent.atomic.AtomicInteger

import org.http4s.Uri
import org.http4s.rho.RhoService
import org.http4s.rho.swagger.SwaggerSupport

import JsonWritable.AutoSerializable
import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task

object MyService extends RhoService with SwaggerSupport {
  import org.http4s.rho._
  import org.http4s.rho.swagger._

  case class JsonResult(name: String, number: Int) extends AutoSerializable

  GET |>> TemporaryRedirect(Uri(path="/hello"))

  // We want to define this chunk of the service as abstract for reuse below
  val hello = GET / "hello"

  "Simple hello world route" **
    hello |>> Ok("Hello world!")

  "A variant of the hello route that takes an Int param" **
    hello / pathVar[Int] |>> { i: Int => Ok(s"You returned $i") }

  "Generates some JSON data from a route param, and a query Int" **
    GET / "result" / pathVar[String] +? param[Int]("id") |>>
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
}
