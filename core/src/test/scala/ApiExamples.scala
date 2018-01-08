
import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import org.http4s.headers.{ETag, `Content-Length`}
import org.http4s.rho._
import org.http4s.rho.bits.TypedQuery
import org.http4s.server.websocket._
import org.http4s.{Request, UrlForm}
import org.specs2.mutable.Specification

class ApiExamples extends Specification {
  "mock api" should {
    "Make it easy to compose routes" in {

      /// src_inlined SimplePath
      new RhoService[IO] {
        GET / "hello" |>> { () => Ok("Hello, world!") }
      }
      /// end_src_inlined

      /// src_inlined ReusePath
      new RhoService[IO] {
        // A path can be built up in multiple steps and the parts reused
        val pathPart1 = GET / "hello"

        pathPart1 / "world" |>> { () => Ok("Hello, world!") }
        pathPart1 / "you"   |>> { () => Ok("Hello, you!") }
      }
      /// end_src_inlined

      /// src_inlined PathCapture
      new RhoService[IO] {
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
      new RhoService[IO] {
        // You can capture the entire rest of the tail using *
        GET / "hello" / * |>> { r: List[String] =>
          Ok(s"Got the rest: ${r.mkString}")
        }
      }
      /// end_src_inlined

      /// src_inlined QueryCapture
      new RhoService[IO] {
        // Query parameters can be captured in a similar manner as path fragments
        GET / "hello" +? param[Int]("fav") |>> { i: Int =>
          Ok(s"Query 'fav' had Int value $i")
        }
      }
      /// end_src_inlined

      /// src_inlined MultiCapture
      new RhoService[IO] {
        // A Path can be made all at once
        POST / pathVar[Int] +? param[Int]("fav") |>> { (i1: Int, i2: Int) =>
          Ok(s"Sum of the number is ${i1 + i2}")
        }
      }
      /// end_src_inlined

      /// src_inlined HeaderCapture
      new RhoService[IO] {
        GET / "hello" >>> capture(ETag) |>> { tag: ETag =>
          Ok(s"Thanks for the tag: $tag")
        }
      }
      /// end_src_inlined

      /// src_inlined HeaderRuleCombine
      new RhoService[IO] {
        // Header rules are composable
        val ensureLength = existsAnd(`Content-Length`)(_.length > 0)
        val getTag = capture(ETag)

        POST / "sequential" >>> getTag >>> ensureLength |>> { tag: ETag =>
          Ok(s"Thanks for the $tag and the non-empty body!")
        }
      }
      /// end_src_inlined

      /// src_inlined BooleanOperators
      new RhoService[IO] {
        /*
         * Boolean logic
         * Just as you can perform 'and' operations which have the effect of
         * building up a path or making mutually required header validations,
         * you can perform 'or' logic with your routes.
         */

        val path1 = "one" / pathVar[Int]
        val path2 = "two" / pathVar[Int]

        val getLength = captureMap(`Content-Length`)(_.length)
        val getTag = captureMap(ETag)(_ => -1l)

        GET / (path1 || path2) +? param[String]("foo") >>> (getLength || getTag) |>> {
          (i: Int, foo: String, v: Long) => Ok(s"Received $i, $foo, $v")
        }
      }
      /// end_src_inlined

      /// src_inlined RequestAccess
      new RhoService[IO] {
        /* Access the `Request` by making it the first param of the
           handler function.
         */
        GET / "request" |>> { req: Request[IO] =>
          Ok("I don't actually need a request...")
        }
        GET / "request" / 'foo |>> { (req: Request[IO], foo: String) =>
          Ok("I wanted a request")
        }
      }
      /// end_src_inlined

      /// src_inlined ResultTypes
      new RhoService[IO] {
        private val counter = new AtomicInteger(0)
        private def getCount(): String = counter.incrementAndGet().toString
        // Don't want status codes? Anything with an `EntityEncoder` will work.
        GET / "nostatus" |>> { () => "No status!" }
        GET / "taskNoStatus" |>> { () =>  IO.pure(getCount()) }

        /* Results need not be functions: they can be anything that has
           an `EntityEncoder` instance in scope */
        GET / "nostatus2" |>> "This is a constant result!"
        GET / "taskNoStatus2" |>> IO.pure(getCount())

        /* We can use a standard http4s.Response, but we don't get any metadata
           with it. Useful for things like Websocket support. */
        GET / "websockets" |>> { () =>
          WebSocketBuilder[IO].build(???, ???)
        }
      }
      /// end_src_inlined

      /// src_inlined StatusCodes
      new RhoService[IO] {
        GET / "twoResults" |>> { () =>
          if (true) Ok("bytes result".getBytes())
          else NotFound("Boo... Not found...")
        }
      }
      /// end_src_inlined

      /// src_inlined Decoders
      new RhoService[IO] {
        // Using decoders you can parse the body as well
        POST / "postSomething" ^ UrlForm.entityDecoder[IO] |>> { m: UrlForm =>
          Ok(s"You posted these things: $m")
        }
      }
      /// end_src_inlined

      /// src_inlined Composed parameters

      new RhoService[IO] {
        import shapeless.{::, HNil}
        case class Foo(i: Int, v: String, a: Double)

        val rawFoo = param[Int]("i") & param[String]("v") & param[Double]("a")

        val paramFoobar: TypedQuery[IO, Foo :: HNil] = rawFoo.map {
          (i: Int, v: String, a: Double) => Foo(i,v,a)
        }

        GET / "foo2-test" +? paramFoobar |>> { (f: Foo) =>
          Ok(s"You queried for foo: $f")
        }
      }

      /// end_src_inlined

      ok
    }
  }

}
