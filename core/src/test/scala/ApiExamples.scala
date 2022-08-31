import cats.effect.IO
import cats.syntax.all._
import munit.FunSuite
import org.http4s.headers.{ETag, `Content-Length`}
import org.http4s.rho.RhoRoutes
import org.http4s.rho.bits.TypedQuery
import org.http4s.{Request, UrlForm}

import java.time.Instant
import java.util.{Date, UUID}
import java.util.concurrent.atomic.AtomicInteger

class ApiExamples extends FunSuite {
  test("A mock API should make it ease to compose routes") {
    /// src_inlined SimplePath
    new RhoRoutes[IO] {
      GET / "hello" |>> { () => Ok("Hello, world!") }
    }
    /// end_src_inlined

    /// src_inlined ReusePath
    new RhoRoutes[IO] {
      // A path can be built up in multiple steps and the parts reused
      val pathPart1 = GET / "hello"

      pathPart1 / "world" |>> { () => Ok("Hello, world!") }
      pathPart1 / "you" |>> { () => Ok("Hello, you!") }
    }
    /// end_src_inlined

    /// src_inlined PathCapture
    new RhoRoutes[IO] {
      // Use combinators to parse and capture path parameters
      GET / "helloworldnumber" / pathVar[Int] / "foo" |>> { i: Int =>
        Ok(s"Received $i")
      }
      // the pv"world" (pv stands for path variable) says 'capture a String' with variable name "world"
      GET / "helloworldstring" / pv"world" / "foo" |>> { i: String =>
        Ok(s"Received $i")
      }
      // capture dates
      GET / "helloworlddate" / pathVar[Date] / "foo" |>> { d: Date =>
        Ok(s"Received $d")
      }
      // capture instants
      GET / "helloworldinstant" / pathVar[Instant] / "foo" |>> { i: Instant =>
        Ok(s"Received $i")
      }
      // capture uuids
      GET / "helloworlduuid" / pathVar[UUID] / "foo" |>> { u: UUID =>
        Ok(s"Received $u")
      }
    }
    /// end_src_inlined

    /// src_inlined CaptureTail
    new RhoRoutes[IO] {
      // You can capture the entire rest of the tail using *
      GET / "hello" / * |>> { r: List[String] =>
        Ok(s"Got the rest: ${r.mkString}")
      }
    }
    /// end_src_inlined

    /// src_inlined QueryCapture
    new RhoRoutes[IO] {
      // Query parameters can be captured in a similar manner as path fragments
      GET / "hello" +? param[Int]("fav") |>> { i: Int =>
        Ok(s"Query 'fav' had Int value $i")
      }
    }
    /// end_src_inlined

    /// src_inlined MultiCapture
    new RhoRoutes[IO] {
      // A Path can be made all at once
      POST / pathVar[Int] +? param[Int]("fav") |>> { (i1: Int, i2: Int) =>
        Ok(s"Sum of the number is ${i1 + i2}")
      }
    }
    /// end_src_inlined

    /// src_inlined HeaderCapture
    new RhoRoutes[IO] {
      GET / "hello" >>> H[ETag].capture |>> { tag: ETag =>
        Ok(s"Thanks for the tag: $tag")
      }
    }
    /// end_src_inlined

    /// src_inlined HeaderRuleCombine
    new RhoRoutes[IO] {
      // Header rules are composable
      val ensureLength = H[`Content-Length`].existsAnd(_.length > 0)
      val getTag = H[ETag].capture

      POST / "sequential" >>> getTag >>> ensureLength |>> { tag: ETag =>
        Ok(s"Thanks for the $tag and the non-empty body!")
      }
    }
    /// end_src_inlined

    /// src_inlined BooleanOperators
    new RhoRoutes[IO] {
      /*
       * Boolean logic
       * Just as you can perform 'and' operations which have the effect of
       * building up a path or making mutually required header validations,
       * you can perform 'or' logic with your routes.
       */

      val path1 = "one" / pathVar[Int]
      val path2 = "two" / pathVar[Int]

      val getLength = H[`Content-Length`].captureMap(_.length)
      val getTag = H[ETag].captureMap(_ => -1L)

      GET / (path1 || path2) +? param[String]("foo") >>> (getLength || getTag) |>> {
        (i: Int, foo: String, v: Long) => Ok(s"Received $i, $foo, $v")
      }
    }
    /// end_src_inlined

    /// src_inlined RequestAccess
    new RhoRoutes[IO] {
      /* Access the `Request` by making it the first param of the
         handler function.
       */
      GET / "request" |>> { _: Request[IO] =>
        Ok("I don't actually need a request...")
      }
      GET / "request" / pv"foo" |>> { (_: Request[IO], _: String) =>
        Ok("I wanted a request")
      }
    }
    /// end_src_inlined

    /// src_inlined ResultTypes
    new RhoRoutes[IO] {
      private val counter = new AtomicInteger(0)
      private def getCount(): String = counter.incrementAndGet().toString
      // Don't want status codes? Anything with an `EntityEncoder` will work.
      GET / "nostatus" |>> { () => "No status!" }
      GET / "taskNoStatus" |>> { () => getCount().pure[IO] }

      /* Results need not be functions: they can be anything that has
         an `EntityEncoder` instance in scope */
      GET / "nostatus2" |>> "This is a constant result!"
      GET / "taskNoStatus2" |>> IO.pure(getCount())

      /* We can use a standard http4s.Response, but we don't get any metadata
         with it. Useful for things like Websocket support. */
//      TODO, how are websockets supported after the changes?
//      GET / "websockets" |>> { () =>
//        WebSocketBuilder[IO].build(???, ???)
//      }
    }
    /// end_src_inlined

    /// src_inlined StatusCodes
    new RhoRoutes[IO] {
      GET / "twoResults" |>> { () =>
        if (true) Ok("bytes result".getBytes())
        else NotFound("Boo... Not found...")
      }
    }
    /// end_src_inlined

    /// src_inlined Decoders
    new RhoRoutes[IO] {
      // Using decoders you can parse the body as well
      POST / "postSomething" ^ UrlForm.entityDecoder[IO] |>> { m: UrlForm =>
        Ok(s"You posted these things: $m")
      }

      // Using decoders after query string
      POST / "postSomething" +? param[String]("foo") ^ UrlForm.entityDecoder[IO] |>> {
        (foo: String, m: UrlForm) =>
          Ok(s"You posted these things ($foo): $m")
      }
    }
    /// end_src_inlined

    /// src_inlined Composed parameters

    new RhoRoutes[IO] {
      import shapeless.{::, HNil}
      case class Foo(i: Int, v: String, a: Double)

      val rawFoo = param[Int]("i") & param[String]("v") & param[Double]("a")

      val paramFoobar: TypedQuery[IO, Foo :: HNil] = rawFoo.map { (i: Int, v: String, a: Double) =>
        Foo(i, v, a)
      }

      GET / "foo2-test" +? paramFoobar |>> { (f: Foo) =>
        Ok(s"You queried for foo: $f")
      }
    }

    /// end_src_inlined
  }
}
