package org.http4s.rho.bits

import org.http4s.Header.{`Content-Type`, Location, `Transfer-Encoding`, `Content-Length`}
import org.http4s._
import org.specs2.mutable.Specification



class ResponseGeneratorSpec extends Specification {
  import ResponseGeneratorInstances._

  "ResponseGenerator" should {
    "Build a response with a body" in {
      val result = Ok("Foo").run
      val resp = result.resp

      val str = new String(resp.body.runLog.run.reduce(_ ++ _).toArray)
      str must_== "Foo"

      resp.headers.get(`Content-Length`) must_== Some(`Content-Length`("Foo".getBytes.length))
      resp.headers.filterNot(_.is(`Content-Length`)).toList must_== EntityEncoder.stringEncoder.headers.toList
    }

    "Build a response without a body" in {
      val result = SwitchingProtocols().run
      val resp = result.resp

      resp.body.runLog.run.length must_== 0
      resp.status must_== Status.SwitchingProtocols
      resp.headers.get(`Content-Length`) must_== None
      resp.headers.get(`Transfer-Encoding`) must_== None
    }

    "Build a redirect response" in {
      val location = Uri.fromString("/foo").getOrElse(sys.error("Fail."))
      val result = MovedPermanently(location).run
      val resp = result.resp

      resp.body.runLog.run.length must_== 0
      resp.status must_== Status.MovedPermanently
      resp.headers.get(`Content-Length`) must_== None
      resp.headers.get(`Transfer-Encoding`) must_== None
      resp.headers.get(Location) must_== Some(Location(location))
    }

    "Explicitly added headers have priority" in {
      val w = EntityEncoder.encodeBy[String](`Content-Type`(MediaType.`text/html`))(EntityEncoder.stringEncoder.toEntity(_))

         Ok("some content", Headers(`Content-Type`(MediaType.`application/json`)))(w)
          .run.resp.headers.get(`Content-Type`).get must_== `Content-Type`(MediaType.`application/json`)
    }
  }
}
