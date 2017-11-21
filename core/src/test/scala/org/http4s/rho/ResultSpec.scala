package org.http4s.rho

import java.time.Instant

import org.http4s.{AttributeKey, HttpDate}
import org.http4s.headers._
import org.specs2.mutable.Specification


class ResultSpec extends Specification with ResultSyntaxInstances {
  import bits.ResponseGeneratorInstances._

  "ResultSyntax" should {
    "Add headers" in {
      val date = Date(HttpDate.Epoch)
      val resp = Ok("Foo")
                  .putHeaders(date)
                  .unsafeRunSync
                  .resp

      resp.headers.get(Date) must beSome(date)

      val respNow = Ok("Foo").unsafeRunSync
        .putHeaders(date)
        .resp

      respNow.headers.get(Date) must beSome(date)
    }

    "Add atributes" in {
      val attrKey = AttributeKey[String]
      val resp = Ok("Foo")
        .withAttribute(attrKey, "foo")
        .unsafeRunSync
        .resp

      resp.attributes.get(attrKey) must beSome("foo")

      val resp2 = Ok("Foo")
        .unsafeRunSync
        .withAttribute(attrKey, "foo")
        .resp

      resp2.attributes.get(attrKey) must beSome("foo")
    }

    "Add a body" in {
      val newbody = "foobar"
      val resp = Ok("foo")
                  .withBody(newbody)
                  .unsafeRunSync
                  .resp

      new String(resp.body.runLogSync.unsafeRunSync.toArray) must_== newbody
      resp.headers.get(`Content-Length`) must beSome(`Content-Length`.unsafeFromLong(newbody.getBytes.length))

      val resp2 = Ok("foo")
        .unsafeRunSync
        .withBody(newbody)
        .unsafeRunSync
        .resp

      new String(resp2.body.runLogSync.unsafeRunSync.toArray) must_== newbody
      resp2.headers.get(`Content-Length`) must beSome(`Content-Length`.unsafeFromLong(newbody.getBytes.length))
    }


  }
}
