package org.http4s
package rhotest

import org.http4s.websocket.WebsocketBits.WebSocketFrame
import org.specs2.mutable.Specification
import org.http4s.rho._
import scalaz.concurrent.Task

import server.websocket.WS




class ApiExamples extends Specification {

  import org.http4s.rho.bits.ResponseGeneratorInstances._

  "mock api" should {
    "Make it easy to compose routes" in {

      new RhoService {
        // the path can be built up in multiple steps and the parts reused
        val path = POST / "hello"
        val path2 = path / 'world +? param[Int]("fav") // the symbol 'world just says 'capture a String'
        path |>> { () => Ok("Empty") } // use the |>> operator to turn a Router into an Action
        path2 |>> { (world: String, fav: Int) => Ok(s"Received $fav, $world") }

        // It can also be made all at once
        val path3 = POST / "hello" / pathVar[Int] +? param[Int]("fav")
        path3 |>> {
          (i1: Int, i2: Int) => Ok(s"Sum of the number is ${i1 + i2}")
        }

        // You can automatically parse variables in the path
        val path4 = GET / "helloworldnumber" / pathVar[Int] / "foo"
        path4 |>> {
          i: Int => Ok("Received $i")
        }

        // You can capture the entire rest of the tail using *
        val path5 = GET / "hello" / * |>> {
          r: List[String] => Ok(s"Got the rest: ${r.mkString}")
        }

        // header validation is also composable
        val v1 = existsAnd(headers.`Content-Length`)(_.length > 0)
        val v2 = v1 && capture(headers.ETag)

        // Now these two can be combined to make the 'Router'
        val r = path2 >>> v2

        // you can continue to add validation actions to a 'Router' but can no longer modify the path
        val r2 = r >>> exists(headers.`Cache-Control`)
        // r2 / "stuff" // Doesn't work

        // Now this can be combined with a method to make the 'Action'
        val action = r2 |>> {
          (world: String, fav: Int, tag: headers.ETag) =>
            Ok("Success").withHeaders(headers.ETag(fav.toString))
        }

        /**
         * Boolean logic
         * Just as you can perform 'and' operations which have the effect of building up a path or
         * making mutually required header validations, you can perform 'or' logic with your routes
         */

        val path6 = "one" / pathVar[Int]
        val path7 = "two" / pathVar[Int]

        val v6 = captureMap(headers.`Content-Length`)(_.length)
        val v7 = captureMap(headers.ETag)(_ => -1)

        GET / (path6 || path7) +? param[String]("foo") >>> (v6 || v7) |>> {
          (i: Int, foo: String, v: Int) =>
            Ok(s"Received $i, $foo, $v")
        }

        // If you want to access the the Request, just add it as the first param
        GET / "getrequest" |>> { req: Request => Ok("Dont need a request") }
        GET / "getrequest" / 'foo |>> { (req: Request, foo: String) => Ok("I wanted a request") }

        // It is not manditory to give a status type
        GET / "nostatus" |>> { () => "No status!"}
        GET / "taskNoStatus" |>> { () => Task("A Future status!") }

        // You can also return a constant result for routes that are invariante of their inputs
        GET / "nostatus2" |>> "This is a constant result!"
        GET / "taskNoStatus2" |>> Task("This task will be evaluated each time!")

        // TODO: this is just a basic example!
        GET /"twoResults" |>> { () =>
          if (true) Ok("bytes result".getBytes())
          else NotFound("Boo... Not found...")
        }

        // Using decoders you can parse the body as well
        POST / "postSomething" ^ UrlForm.entityDecoder |>> { m: UrlForm =>
          Ok(s"You posted these things: $m")
        }

        // We can use a standard http4s.Response, but we don't get any metadata along with it. Useful for things
        // like Websocket support.
        GET / "websockets" |>> {
          val exchange: scalaz.stream.Exchange[WebSocketFrame,WebSocketFrame] = null
          WS(exchange)
        }

        // We can add filters to an action with the '^?>' operator
        val myFilter = RequestFilter.pureFilter(req => if (req.params.get("foo").nonEmpty) None else Some(Conflict("I'm so conflicted...")))
        GET / "filtered" |>> myFilter ^?> Ok("You had a foo param")
      }

      ok
    }
  }
}
