package org.http4s
package rhotest

import org.http4s.websocket.WebsocketBits.WebSocketFrame
import org.specs2.mutable.Specification
import org.http4s.rho._
import scalaz.concurrent.Task

import org.http4s.headers.{`Content-Length`, ETag}
import server.websocket.WS




class ApiExamples extends Specification {

  "mock api" should {
    "Make it easy to compose routes" in {

      /// src_inlined SimplePath
      new RhoService {
        // A Path can be made all at once
        POST / "hello" / pathVar[Int] +? param[Int]("fav") |>> { (i1: Int, i2: Int) =>
          Ok(s"Sum of the number is ${i1 + i2}")
        }
      }
      /// end_src_inlined

      /// src_inlined ReusePath
      new RhoService {
        // A path can be built up in multiple steps and the parts reused
        val pathPart1 = POST / "hello"
        val pathPart2 = pathPart1 / 'world +? param[Int]("fav") // the symbol 'world just says 'capture a String'
        pathPart1 |>> { () => Ok("Empty") } // use the |>> operator to turn a Router into an Action
        pathPart2 |>> { (world: String, fav: Int) => Ok(s"Received $fav, $world") }
      }
      /// end_src_inlined

      /// src_inlined PathCapture
      new RhoService {
        // Use combinators to parse and capture path parameters
        GET / "helloworldnumber" / pathVar[Int] / "foo" |>> { i: Int =>
          Ok("Received $i")
        }
      }
      /// end_src_inlined

      /// src_inlined CaptureTail
      new RhoService {
        // You can capture the entire rest of the tail using *
        GET / "hello" / * |>> { r: List[String] => Ok(s"Got the rest: ${r.mkString}") }
      }
      /// end_src_inlined

      /// src_inlined HeaderCapture
      new RhoService {
        GET / "hello" >>> capture(ETag) |>> { tag: ETag =>
          Ok(s"Thanks for the tag: $tag")
        }
      }
      /// end_src_inlined

      /// src_inlined HeaderRuleCombine
      new RhoService {
        // Header rules are composable
        val ensureLength = existsAnd(`Content-Length`)(_.length > 0)
        val getTag = capture(ETag)

        POST / "sequential" >>> getTag >>> ensureLength |>> { tag: ETag =>
          Ok(s"Thanks for the $tag and the body with Content-Length greater than 0!")
        }
      }
      /// end_src_inlined

      /// src_inlined BooleanOperators
      new RhoService {
        /*
         * Boolean logic
         * Just as you can perform 'and' operations which have the effect of building up a path or
         * making mutually required header validations, you can perform 'or' logic with your routes
         */

        val path1 = "one" / pathVar[Int]
        val path2 = "two" / pathVar[Int]

        val getLength = captureMap(`Content-Length`)(_.length)
        val getTag = captureMap(ETag)(_ => -1)

        GET / (path1 || path2) +? param[String]("foo") >>> (getLength || getTag) |>> {
          (i: Int, foo: String, v: Int) =>
            Ok(s"Received $i, $foo, $v")
        }
      }
      /// end_src_inlined

      /// src_inlined RequestAccess
      new RhoService {
        // If you want to access the the Request, just add it as the first param
        GET / "getrequest" |>> { req: Request => Ok("Dont need a request") }
        GET / "getrequest" / 'foo |>> { (req: Request, foo: String) => Ok("I wanted a request") }
      }
      /// end_src_inlined

      /// src_inlined ResultTypes
      new RhoService {
        // Status codes are not mandatory: anything with an `EntityEncoder` will work.
        GET / "nostatus" |>> { () => "No status!" }
        GET / "taskNoStatus" |>> { () => Task("A Future status!") }

        /* Results need not be functions: they can be anything that has
           an `EntityEncoder` instance in scope */
        GET / "nostatus2" |>> "This is a constant result!"
        GET / "taskNoStatus2" |>> Task("This task will be evaluated each time!")

        /* We can use a standard http4s.Response, but we don't get any metadata
           with it. Useful for things like Websocket support. */
        GET / "websockets" |>> { () =>
          val exchange: scalaz.stream.Exchange[WebSocketFrame,WebSocketFrame] = ???
          WS(exchange)
        }
      }
      /// end_src_inlined

      /// src_inlined StatusCodes
      new RhoService {
        GET / "twoResults" |>> { () =>
          if (true) Ok("bytes result".getBytes())
          else NotFound("Boo... Not found...")
        }
      }
      /// end_src_inlined

      /// src_inlined Decoders
      new RhoService {
        // Using decoders you can parse the body as well
        POST / "postSomething" ^ UrlForm.entityDecoder |>> { m: UrlForm =>
          Ok(s"You posted these things: $m")
        }
      }
      /// end_src_inlined


      ok
    }
  }

}
