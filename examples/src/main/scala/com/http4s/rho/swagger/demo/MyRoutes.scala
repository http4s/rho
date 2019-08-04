package com.http4s.rho.swagger.demo

import cats.Monad
import cats.effect.Effect
import cats.implicits._
import com.http4s.rho.swagger.demo.JsonEncoder.{AutoSerializable, _}
import com.http4s.rho.swagger.demo.MyRoutes._
import fs2.Stream
import org.http4s.rho.RhoRoutes
import org.http4s.rho.bits._
import org.http4s.rho.swagger.{SwaggerFileResponse, SwaggerSyntax}
import org.http4s.{EntityDecoder, Headers, HttpDate, Request, ResponseCookie, Uri, headers}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import shapeless.HNil

import scala.reflect.ClassTag
import scala.collection.immutable.Seq

abstract class MyRoutes[F[+_] : Effect](swaggerSyntax: SwaggerSyntax[F])
  extends RhoRoutes[F] {

  import swaggerSyntax._

  val requireCookie: TypedHeader[F, HNil] = existsAndR(headers.Cookie){ cookie =>
    cookie.values.toList.find(c => c.name == "Foo" && c.content == "bar") match {
      case Some(_) => // Cookie found, good to go
        None
      case None =>    // Didn't find cookie
        Some(TemporaryRedirect(Uri(path = "/addcookie")).widen)
    }
  }

  "We don't want to have a real 'root' route anyway... " **
    GET |>> TemporaryRedirect(Uri(path = "/swagger-ui"))

  // We want to define this chunk of the service as abstract for reuse below
  val hello = "hello" @@ GET / "hello"

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

  // Normally this would be part of the constructor since its creation is 'unsafe'
  private val counterRef = cats.effect.concurrent.Ref.unsafe[F, Int](0)

  "This uses a simple counter for the number of times this route has been requested" **
    POST / "counter" |>> { () =>
      counterRef.modify { i =>
          val inc = i + 1
          (inc, inc)
        }
        .map { counter => (
          s"The number is ${counter}")
        }
    }

  "Adds the cookie Foo=bar to the client" **
  "cookies" @@
    GET / "addcookie" |>> {
      Ok("You now have a good cookie!").map(_.addCookie("Foo", "bar"))
    }

  "Sets the cookie Foo=barr to the client" **
  "cookies" @@
    GET / "addbadcookie" |>> {
      Ok("You now have an evil cookie!").map(_.addCookie("Foo", "barr"))
    }

  "Checks the Foo cookie to make sure its 'bar'" **
  "cookies" @@
    GET / "checkcookie" >>> requireCookie |>> Ok("Good job, you have the cookie!")

  "Clears the cookies" **
  "cookies" @@
    GET / "clearcookies" |>> { req: Request[F] =>
      val hs = req.headers.get(headers.Cookie) match {
        case None => Headers.empty
        case Some(cookie) =>
          Headers(cookie.values.toList.map { c => headers.`Set-Cookie`(ResponseCookie(c.name, c.content, expires = Some(HttpDate.Epoch), maxAge = Some(0)))})
      }

      Ok("Deleted cookies!").map(_.withHeaders(hs))
    }

    "This route allows your to post stuff" **
    List("post", "stuff") @@
      POST / "post" ^ EntityDecoder.text[F] |>> { body: String =>
      "You posted: " + body
    }

  "This route allows your to post stuff with query parameters" **
  List("post", "stuff", "query") @@
    POST / "post-query" +? param[String]("query") ^ EntityDecoder.text[F] |>> { (query: String, body: String) =>
    s"You queried '$query' and posted: $body"
  }

  "This demonstrates using a process of entities" **
    GET / "stream" |>> {
      val s = 0 until 100 map (i => s"Hello $i\n")
      val p: Stream[F, String] = Stream.emits(s).covary[F]

      Ok(p)
    }

  "Get a file" **
    GET / "file" |>> Ok(SwaggerFileResponse("HELLO"))

  "This route demonstrates how to use a complex data type as parameters in route" **
  GET / "complex" +? param[Foo]("foo") & param[Seq[Bar]]("bar", Nil) |>> { (foo: Foo, bars: Seq[Bar]) =>
    Ok(s"Received foo: $foo, bars: $bars")
  }
}

object MyRoutes {
  import scala.reflect.runtime.universe.TypeTag

  case class Foo(k: String, v: Int)
  case class Bar(id: Long, foo: Foo)

  case class JsonResult(name: String, number: Int) extends AutoSerializable

  private implicit val format: DefaultFormats =
    DefaultFormats

  implicit def jsonParser[F[_], A : TypeTag : ClassTag]: StringParser[F, A] = new StringParser[F, A] with FailureResponseOps[F] {
    override val typeTag: Option[TypeTag[A]] =
      implicitly[TypeTag[A]].some

    override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, A] = {

      Either.catchNonFatal(JsonMethods.parse(s).extract[A]) match {
        case Left(t) => badRequest[String](t.getMessage)
        case Right(t) => SuccessResponse(t)
      }
    }
  }
}
