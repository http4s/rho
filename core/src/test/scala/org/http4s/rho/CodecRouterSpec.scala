package org.http4s
package rho

import cats.effect.IO
import fs2.Stream
import org.specs2.mutable.Specification

class CodecRouterSpec extends Specification {

  def bodyAndStatus(resp: Response[IO]): (String, Status) = {
    val rbody = new String(resp.body.compile.toVector.unsafeRunSync.foldLeft(Array[Byte]())(_ :+ _))
    (rbody, resp.status)
  }

  "A CodecRouter in a RhoService" should {

    val routes = new RhoService[IO] {
      (POST / "foo" decoding(EntityDecoder.text[IO])) |>> { s: String => Ok(s"Received: $s") }
      (POST / "form" decoding(UrlForm.entityDecoder[IO])) |>> { m: UrlForm => Ok("success") }
    }.toRoutes()

    "Decode a valid body" in {

      val b = Stream.emits("hello".getBytes)
      val h = Headers(headers.`Content-Type`(MediaType.text.plain))
      val req = Request[IO](Method.POST, Uri(path = "/foo"), headers = h, body = b)
      val result = routes(req).value.unsafeRunSync().getOrElse(Response.notFound)
      val (bb, s) = bodyAndStatus(result)

      s must_== Status.Ok
      bb must_== "Received: hello"
    }

    "Fail on invalid body" in {
      val b = Stream.emits("hello =".getBytes)
      val h = Headers(headers.`Content-Type`(MediaType.application.`x-www-form-urlencoded`))
      val req = Request[IO](Method.POST, Uri(path = "/form"), headers = h, body = b)

      routes(req).value.unsafeRunSync().map(_.status) must be some Status.BadRequest
    }
  }
}
