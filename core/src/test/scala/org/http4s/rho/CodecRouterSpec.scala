package org.http4s
package rho

import org.http4s.EntityDecoder.DecodingException
import org.specs2.mutable.Specification

import scodec.bits.ByteVector
import scalaz.stream.Process

class CodecRouterSpec extends Specification {

  def bodyAndStatus(resp: Response): (String, Status) = {
    val rbody = new String(resp.body.runLog.run.reduce(_ ++ _).toArray)
    (rbody, resp.status)
  }

  "A CodecRouter in a RhoService" should {

    val service = new RhoService {
      (POST / "foo" decoding(EntityDecoder.text)) |>> { s: String => Ok(s"Received: $s") }
      (POST / "form" decoding(EntityDecoder.formEncoded)) |>> { m: Map[String, Seq[String]] => Ok("success")}
    }.toService

    "Decode a valid body" in {

      val b = Process.emit(ByteVector.view("hello".getBytes))
      val h = Headers(Header.`Content-Type`(MediaType.`text/plain`))
      val req = Request(Method.POST, Uri(path = "/foo"), headers = h, body = b)
      val result = service(req).run.get
      val (bb, s) = bodyAndStatus(result)

      s must_== Status.Ok
      bb must_== "Received: hello"
    }

    "Fail on missing header" in {

      val b = Process.emit(ByteVector.view("hello".getBytes))
      val req = Request(Method.POST, Uri(path = "/foo"),body = b)
      val result = service(req).run.get
      val (bb, s) = bodyAndStatus(result)

      s must_== Status.BadRequest
      bb must_== "Missing header: " + Header.`Content-Type`.name
    }

    "Fail on invalid body" in {
      val b = Process.emit(ByteVector.view("hello =".getBytes))
      val h = Headers(Header.`Content-Type`(MediaType.`application/x-www-form-urlencoded`))
      val req = Request(Method.POST, Uri(path = "/form"), headers = h, body = b)

      service(req).run.get must throwA[DecodingException]
    }
  }

}
