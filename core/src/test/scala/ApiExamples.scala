
import java.util.concurrent.atomic.AtomicInteger

import org.http4s.headers.{ETag, `Content-Length`}
import org.http4s.rho._
import org.http4s.server.websocket._
import org.http4s.websocket.WebsocketBits.WebSocketFrame
import org.http4s.{Request, UrlForm}
import org.specs2.mutable.Specification

import scalaz.concurrent.Task
import scalaz.stream


class ApiExamples extends Specification {

  "mock api" should {
    "Make it easy to compose routes" in {

      /// src_inlined SimplePath
      new RhoService {
        GET / "hello" |>> { () => Ok("Hello, world!") }
      }
      /// end_src_inlined

      /// src_inlined ReusePath
      new RhoService {
        // A path can be built up in multiple steps and the parts reused
        val pathPart1 = GET / "hello"

        pathPart1 / "world" |>> { () => Ok("Hello, world!") }
        pathPart1 / "you"   |>> { () => Ok("Hello, you!") }
      }
      /// end_src_inlined

      /// src_inlined PathCapture
      new RhoService {
        // Use combinators to parse and capture path parameters
        GET / "helloworldnumber" / pathVar[Int] / "foo" |>> { i: Int =>
          Ok("Received $i")
        }
        // the symbol 'world just says 'capture a String' with variable name "world"
        GET / "helloworldstring" / 'world / "foo" |>> { i: String =>
          Ok("Received $i")
        }
      }
      /// end_src_inlined

      /// src_inlined CaptureTail
      new RhoService {
        // You can capture the entire rest of the tail using *
        GET / "hello" / * |>> { r: List[String] =>
          Ok(s"Got the rest: ${r.mkString}")
        }
      }
      /// end_src_inlined

      /// src_inlined QueryCapture
      new RhoService {
        // Query parameters can be captured in a similar manner as path fragments
        GET / "hello" +? param[Int]("fav") |>> { i: Int =>
          Ok(s"Query 'fav' had Int value $i")
        }
      }
      /// end_src_inlined

      /// src_inlined MultiCapture
      new RhoService {
        // A Path can be made all at once
        POST / pathVar[Int] +? param[Int]("fav") |>> { (i1: Int, i2: Int) =>
          Ok(s"Sum of the number is ${i1 + i2}")
        }
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
          Ok(s"Thanks for the $tag and the non-empty body!")
        }
      }
      /// end_src_inlined

      /// src_inlined BooleanOperators
      new RhoService {
        /*
         * Boolean logic
         * Just as you can perform 'and' operations which have the effect of
         * building up a path or making mutually required header validations,
         * you can perform 'or' logic with your routes.
         */

        val path1 = "one" / pathVar[Int]
        val path2 = "two" / pathVar[Int]

        val getLength = captureMap(`Content-Length`)(_.length)
        val getTag = captureMap(ETag)(_ => -1)

        GET / (path1 || path2) +?
            param[String]("foo") >>>
            (getLength || getTag) |>> {
          (i: Int, foo: String, v: Int) =>
            Ok(s"Received $i, $foo, $v")
        }
      }
      /// end_src_inlined

      /// src_inlined RequestAccess
      new RhoService {
        /* Access the `Request` by making it the first param of the
           handler function.
         */
        GET / "request" |>> { req: Request =>
          Ok("I don't acutally need a request...")
        }
        GET / "request" / 'foo |>> { (req: Request, foo: String) =>
          Ok("I wanted a request")
        }
      }
      /// end_src_inlined

      /// src_inlined ResultTypes
      new RhoService {
        private val counter = new AtomicInteger(0)
        private def getCount(): String = counter.incrementAndGet().toString
        // Don't want status codes? Anything with an `EntityEncoder` will work.
        GET / "nostatus" |>> { () => "No status!" }
        GET / "taskNoStatus" |>> { () => Task(getCount())
        }

        /* Results need not be functions: they can be anything that has
           an `EntityEncoder` instance in scope */
        GET / "nostatus2" |>> "This is a constant result!"
        GET / "taskNoStatus2" |>> Task(getCount())

        /* We can use a standard http4s.Response, but we don't get any metadata
           with it. Useful for things like Websocket support. */
        GET / "websockets" |>> { () =>
          val exchange: stream.Exchange[WebSocketFrame,WebSocketFrame] = ???
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
