package org.http4s
package rho

import org.specs2.mutable.Specification

import scodec.bits.ByteVector
import fs2.Stream

class CodecRouterSpec extends Specification {

  def bodyAndStatus(resp: Response): (String, Status) = {
    val rbody = new String(resp.body.runLog.unsafeRun.foldLeft(ByteVector.empty)(_ :+ _).toArray)
    (rbody, resp.status)
  }

  "A CodecRouter in a RhoService" should {

    val service = new RhoService {
      (POST / "foo" decoding(EntityDecoder.text)) |>> { s: String => Ok(s"Received: $s") }
      (POST / "form" decoding(UrlForm.entityDecoder)) |>> { m: UrlForm => Ok("success")}
    }.toService()

    "Decode a valid body" in {

      val b = Stream.emits("hello".getBytes)
      val h = Headers(headers.`Content-Type`(MediaType.`text/plain`))
      val req = Request(Method.POST, Uri(path = "/foo"), headers = h, body = b)
      val result = service(req).unsafeRun.orNotFound
      val (bb, s) = bodyAndStatus(result)

      s must_== Status.Ok
      bb must_== "Received: hello"
    }

    "Fail on invalid body" in {
      val b = Stream.emits("hello =".getBytes)
      val h = Headers(headers.`Content-Type`(MediaType.`application/x-www-form-urlencoded`))
      val req = Request(Method.POST, Uri(path = "/form"), headers = h, body = b)

      service(req).unsafeRun.orNotFound.status must_== Status.BadRequest
    }
  }

}
