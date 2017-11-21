package org.http4s.rho.bits

import org.http4s.headers.{Location, `Content-Length`, `Content-Type`, `Transfer-Encoding`}
import org.http4s._
import org.specs2.mutable.Specification
import scodec.bits.ByteVector
import cats.effect.IO

class ResponseGeneratorSpec extends Specification {
  import ResponseGeneratorInstances._

  "ResponseGenerator" should {
    "Build a response with a body" in {
      val result = Ok("Foo").unsafeRunSync
      val resp = result.resp

      val str = new String(resp.body.runLogSync.unsafeRunSync.foldLeft(ByteVector.empty)(_ :+ _).toArray)
      str must_== "Foo"

      resp.headers.get(`Content-Length`) must beSome(`Content-Length`.unsafeFromLong("Foo".getBytes.length))
      resp.headers.filterNot(_.is(`Content-Length`)).toList must_== EntityEncoder.stringEncoder.headers.toList
    }

    "Build a response without a body" in {
      val result = SwitchingProtocols().unsafeRunSync
      val resp = result.resp

      resp.body.runLogSync.unsafeRunSync.length must_== 0
      resp.status must_== Status.SwitchingProtocols
      resp.headers.get(`Content-Length`) must beNone
      resp.headers.get(`Transfer-Encoding`) must beNone
    }

    "Build a redirect response" in {
      val location = Uri.fromString("/foo").right.getOrElse(sys.error("Fail."))
      val result = MovedPermanently(location).unsafeRunSync
      val resp = result.resp

      resp.body.runLogSync.unsafeRunSync.length must_== 0
      resp.status must_== Status.MovedPermanently
      resp.headers.get(`Content-Length`) must beNone
      resp.headers.get(`Transfer-Encoding`) must beNone
      resp.headers.get(Location) must beSome(Location(location))
    }

    "Explicitly added headers have priority" in {
      val w = EntityEncoder.encodeBy[IO, String](`Content-Type`(MediaType.`text/html`))(EntityEncoder.stringEncoder[IO].toEntity(_))

         Ok("some content", Headers(`Content-Type`(MediaType.`application/json`)))(w)
          .unsafeRunSync.resp.headers.get(`Content-Type`).get must_== `Content-Type`(MediaType.`application/json`)
    }
  }
}
