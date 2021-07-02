package org.http4s
package rho

import cats.effect.IO
import fs2.Stream
import munit.FunSuite

import scala.collection.compat.immutable.ArraySeq

class CodecRouterSuite extends FunSuite {
  import cats.effect.unsafe.implicits.global

  def bodyAndStatus(resp: Response[IO]): (String, Status) = {
    val rbody = new String(
      resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _)
    )
    (rbody, resp.status)
  }

  val routes = new RhoRoutes[IO] {
    (POST / "foo" decoding (EntityDecoder.text[IO])) |>> { s: String => Ok(s"Received: $s") }
    (POST / "form" decoding (UrlForm.entityDecoder[IO])) |>> { _: UrlForm => Ok("success") }
  }.toRoutes()

  test("A CodecRouter in a RhoRoutes should decode a valid body") {
    val b = Stream.emits(ArraySeq.unsafeWrapArray("hello".getBytes))
    val h = Headers(headers.`Content-Type`(MediaType.text.plain))
    val req = Request[IO](Method.POST, uri"/foo", headers = h, body = b)
    val result = routes(req).value.unsafeRunSync().getOrElse(Response.notFound)
    val (bb, s) = bodyAndStatus(result)

    assertEquals(s, Status.Ok)
    assertEquals(bb, "Received: hello")
  }

  test("A CodecRouter in a RhoRoutes should fail on invalid body") {
    val b = Stream.emits(ArraySeq.unsafeWrapArray("hello =".getBytes))
    val h = Headers(headers.`Content-Type`(MediaType.application.`x-www-form-urlencoded`))
    val req =
      Request[IO](Method.POST, uri"/form", headers = h, body = b)

    assertEquals(routes(req).value.unsafeRunSync().map(_.status), Some(Status.BadRequest))
  }
}
