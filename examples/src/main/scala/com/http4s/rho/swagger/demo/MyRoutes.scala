package com.http4s.rho.swagger.demo

import cats.Monad
import cats.effect._
import cats.implicits._
import com.http4s.rho.swagger.demo.MyRoutes._
import fs2.Stream
import org.http4s.rho.RhoRoutes
import org.http4s.rho.bits._
import org.http4s.rho.swagger.{SwaggerFileResponse, SwaggerSyntax}
import org.http4s.{EntityDecoder, Headers, HttpDate, Request, ResponseCookie, headers}
import shapeless.HNil
import org.http4s.circe.CirceEntityEncoder
import org.http4s.implicits._

class MyRoutes[F[+_]: Effect](swaggerSyntax: SwaggerSyntax[F])
    extends RhoRoutes[F]
    with CirceEntityEncoder {
  import swaggerSyntax._

  val requireCookie: TypedHeader[F, HNil] = H[headers.Cookie].existsAndR { cookie =>
    cookie.values.toList.find(c => c.name == "Foo" && c.content == "bar") match {
      case Some(_) => // Cookie found, good to go
        None
      case None => // Didn't find cookie
        Some(TemporaryRedirect(uri"/addcookie").widen)
    }
  }

  "We don't want to have a real 'root' route anyway... " **
    GET |>> TemporaryRedirect(uri"/swagger-ui")

  // We want to define this chunk of the service as abstract for reuse below
  val hello = "hello" @@ GET / "hello"

  "Simple hello world route" **
    hello |>> Ok("Hello world!")

  "A variant of the hello route that takes an Int param" **
    hello / pathVar[Int] |>> { i: Int => Ok(s"You returned $i") }

  "This route allows you to send head request" **
    HEAD / "hello" |>> Ok("Hello head!")

  "Generates some JSON data from a route param, and a query Int" **
    GET / "result" / pv"foo" +? param[Int]("id") |>> { (name: String, id: Int) =>
      Ok(JsonResult(name, id))
    }

  "Two different response codes can result from this route based on the number given" **
    GET / "differentstatus" / pathVar[Int] |>> { i: Int =>
    if (i >= 0) Ok(JsonResult("Good result", i))
    else BadRequest(s"Negative number: $i")
  }

  // Normally this would be part of the constructor since its creation is 'unsafe'
  private val counterRef = cats.effect.concurrent.Ref.unsafe[F, Int](0)

  "This uses a simple counter for the number of times this route has been requested" **
    POST / "counter" |>> { () =>
      counterRef
        .modify { i =>
          val inc = i + 1
          (inc, inc)
        }
        .map { counter =>
          s"The number is ${counter}"
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
      val hs = req.headers.get[headers.Cookie] match {
        case None => Headers.empty
        case Some(cookie) =>
          Headers(cookie.values.toList.map { c =>
            headers.`Set-Cookie`(
              ResponseCookie(c.name, c.content, expires = Some(HttpDate.Epoch), maxAge = Some(0))
            )
          })
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
    POST / "post-query" +? param[String]("query") ^ EntityDecoder.text[F] |>> {
      (query: String, body: String) =>
        s"You queried '$query' and posted: $body"
    }

  "This demonstrates using a process of entities" **
    GET / "stream" |>> {
      val s = (0 until 100).map(i => s"Hello $i\n")
      val p: Stream[F, String] = Stream.emits(s).covary[F]

      Ok(p)
    }

  "Get a file" **
    GET / "file" |>> Ok(SwaggerFileResponse("HELLO"))

  "This route demonstrates how to use a complex data type as parameters in route" **
    GET / "complex" +? param[Foo]("foo") & param[Bar]("bar", Bar(0)) |>> { (foo: Foo, bars: Bar) =>
      Ok(s"Received foo: $foo, bars: $bars")
    }
}

object MyRoutes {

  case class Foo(key: String, value: String)

  implicit def fooParser[F[_]: Monad]: StringParser[F, Foo] = {
    val fooRegex = """([^_]+)_([^_]+)""".r
    StringParser.strParser[F].rmap {
      case fooRegex(k, v) => SuccessResponse(Foo(k, v))
      case other =>
        FailureResponse.pure[F](
          FailureResponseOps[F].BadRequest
            .pure(s"""Invalid value "$other". Expected "{key}_{value}".""")
        )
    }
  }

  case class Bar(i: Int) extends AnyVal

  implicit def barParser[F[_]]: StringParser[F, Bar] =
    StringParser.intParser[F].map(Bar)

  case class JsonResult(name: String, number: Int)
  implicit val jsonResultCodec: io.circe.Codec[JsonResult] = io.circe.generic.semiauto.deriveCodec
}
