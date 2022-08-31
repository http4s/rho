package org.http4s
package rho

import cats.effect.IO
import fs2.Stream
import munit.CatsEffectSuite

import scala.collection.compat.immutable.ArraySeq

class CodecRouterSuite extends CatsEffectSuite {
  private def bodyAndStatus(resp: Response[IO]): IO[(String, Status)] = {
    val rbody = resp.body.compile.toVector.map(_.foldLeft(Array[Byte]())(_ :+ _)).map(new String(_))
    rbody.map(_ -> resp.status)
  }

  val routes = new RhoRoutes[IO] {
    (POST / "foo" decoding (EntityDecoder.text[IO])) |>> { s: String => Ok(s"Received: $s") }
    (POST / "form" decoding (UrlForm.entityDecoder[IO])) |>> { _: UrlForm => Ok("success") }
  }.toRoutes()

  test("A CodecRouter in a RhoRoutes should decode a valid body") {
    val b = Stream.emits(ArraySeq.unsafeWrapArray("hello".getBytes))
    val h = Headers(headers.`Content-Type`(MediaType.text.plain))
    val req = Request[IO](Method.POST, uri"/foo", headers = h, entity = Entity(b))

    for {
      result <- routes(req).value.map(_.getOrElse(Response.notFound))
      _ <- assertIO(bodyAndStatus(result), "Received: hello" -> Status.Ok)
    } yield ()
  }

  test("A CodecRouter in a RhoRoutes should fail on invalid body") {
    val b = Stream.emits(ArraySeq.unsafeWrapArray("hello =".getBytes))
    val h = Headers(headers.`Content-Type`(MediaType.application.`x-www-form-urlencoded`))
    val req =
      Request[IO](Method.POST, uri"/form", headers = h, entity = Entity(b))

    assertIO(routes(req).value.map(_.map(_.status)), Some(Status.BadRequest))
  }
}
