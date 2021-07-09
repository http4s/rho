package org.http4s.rho.bits

import cats.effect.IO
import munit.FunSuite
import org.http4s._
import org.http4s.headers.{Location, `Content-Length`, `Content-Type`, `Transfer-Encoding`}
import org.http4s.rho.io._

class ResponseGeneratorSuite extends FunSuite {
  import cats.effect.unsafe.implicits.global

  test("A ResponseGenerator should build a response with a body") {
    val result = Ok("Foo").unsafeRunSync()
    val resp = result.resp

    val str =
      new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
    assertEquals(str, "Foo")

    assertEquals(
      resp.headers.get[`Content-Length`],
      Some(
        `Content-Length`.unsafeFromLong("Foo".getBytes.length)
      )
    )
    assertEquals(
      resp.headers.headers
        .filterNot(_.name == `Content-Length`.name),
      EntityEncoder.stringEncoder.headers.headers
    )
  }

  test("A ResponseGenerator should build a response without a body") {
    val result = SwitchingProtocols.apply.unsafeRunSync()
    val resp = result.resp

    assertEquals(resp.body.compile.toVector.unsafeRunSync().length, 0)
    assertEquals(resp.status, Status.SwitchingProtocols)
    assertEquals(resp.headers.get[`Content-Length`], None)
    assertEquals(resp.headers.get[`Transfer-Encoding`], None)
  }

  test("A ResponseGenerator should build a redirect response") {
    val location = Uri.fromString("/foo").getOrElse(sys.error("Fail."))
    val result = MovedPermanently(location).unsafeRunSync()
    val resp = result.resp

    assertEquals(resp.body.compile.toVector.unsafeRunSync().length, 0)
    assertEquals(resp.status, Status.MovedPermanently)
    assertEquals(resp.headers.get[`Content-Length`], None)
    assertEquals(resp.headers.get[`Transfer-Encoding`], None)
    assertEquals(resp.headers.get[Location], Some(Location(location)))
  }

  test("A ResponseGenerator should build a redirect response with a body") {
    val testBody = "Moved!!!"
    val location = Uri.fromString("/foo").getOrElse(sys.error("Fail."))
    val result = MovedPermanently(location, testBody).unsafeRunSync()
    val resp = result.resp

    assertEquals(
      EntityDecoder[IO, String].decode(resp, false).value.unsafeRunSync(),
      Right(testBody)
    )
    assertEquals(resp.status, Status.MovedPermanently)
    assertEquals(
      resp.headers.get[`Content-Length`],
      Some(
        `Content-Length`.unsafeFromLong(testBody.length)
      )
    )
    assertEquals(resp.headers.get[`Transfer-Encoding`], None)
    assertEquals(resp.headers.get[Location], Some(Location(location)))
  }

  test("A ResponseGenerator should explicitly added headers have priority") {
    implicit val w: EntityEncoder[IO, String] =
      EntityEncoder.encodeBy[IO, String](`Content-Type`(MediaType.text.html))(
        EntityEncoder.stringEncoder[IO].toEntity(_)
      )

    assertEquals(
      Ok("some content", Headers(`Content-Type`(MediaType.application.json)))
        .unsafeRunSync()
        .resp
        .headers
        .get[`Content-Type`]
        .get,
      `Content-Type`(MediaType.application.json)
    )
  }
}
