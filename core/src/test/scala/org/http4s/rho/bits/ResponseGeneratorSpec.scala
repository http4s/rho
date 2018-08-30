package org.http4s.rho.bits

import cats.effect.IO
import org.http4s._
import org.http4s.headers.{Location, `Content-Length`, `Content-Type`, `Transfer-Encoding`}
import org.http4s.rho.io._
import org.specs2.mutable.Specification

class ResponseGeneratorSpec extends Specification {
  "ResponseGenerator" should {
    "Build a response with a body" in {
      val result = Ok("Foo").unsafeRunSync()
      val resp = result.resp

      val str = new String(resp.body.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
      str must_== "Foo"

      resp.headers.get(`Content-Length`) must beSome(`Content-Length`.unsafeFromLong("Foo".getBytes.length))
      resp.headers.filterNot(_.is(`Content-Length`)).toList must_== EntityEncoder.stringEncoder.headers.toList
    }

    "Build a response without a body" in {
      val result = SwitchingProtocols.apply.unsafeRunSync()
      val resp = result.resp

      resp.body.compile.toVector.unsafeRunSync().length must_== 0
      resp.status must_== Status.SwitchingProtocols
      resp.headers.get(`Content-Length`) must beNone
      resp.headers.get(`Transfer-Encoding`) must beNone
    }

    "Build a redirect response" in {
      val location = Uri.fromString("/foo").right.getOrElse(sys.error("Fail."))
      val result = MovedPermanently(location).unsafeRunSync()
      val resp = result.resp

      resp.body.compile.toVector.unsafeRunSync().length must_== 0
      resp.status must_== Status.MovedPermanently
      resp.headers.get(`Content-Length`) must beNone
      resp.headers.get(`Transfer-Encoding`) must beNone
      resp.headers.get(Location) must beSome(Location(location))
    }

    "Explicitly added headers have priority" in {
      implicit val w: EntityEncoder[IO, String] =
        EntityEncoder.encodeBy[IO, String](`Content-Type`(MediaType.text.html))(EntityEncoder.stringEncoder[IO].toEntity(_))

      Ok("some content", Headers(`Content-Type`(MediaType.application.json)))
        .unsafeRunSync().resp.headers.get(`Content-Type`).get must_== `Content-Type`(MediaType.application.json)
    }
  }
}
