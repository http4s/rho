package org.http4s.rho

import org.typelevel.vault._
import cats.effect._
import org.http4s.headers._
import org.http4s.rho.io._
import org.http4s.HttpDate
import org.specs2.mutable.Specification
import cats.effect.unsafe.implicits.global

class ResultSpec extends Specification {

  "ResultSyntax" should {
    "Add headers" in {
      val date = Date(HttpDate.Epoch)
      val resp = Ok("Foo")
        .map(_.putHeaders(date))
        .unsafeRunSync()
        .resp

      resp.headers.get[Date] must beSome(date)

      val respNow = Ok("Foo")
        .map(_.putHeaders(date))
        .unsafeRunSync()
        .resp

      respNow.headers.get[Date] must beSome(date)
    }

    "Add atributes" in {
      val attrKey = Key.newKey[SyncIO, String].unsafeRunSync()
      val resp = Ok("Foo")
        .map(_.withAttribute(attrKey, "foo"))
        .unsafeRunSync()
        .resp

      resp.attributes.lookup(attrKey) must beSome("foo")

      val resp2 = Ok("Foo")
        .map(_.withAttribute(attrKey, "foo"))
        .unsafeRunSync()
        .resp

      resp2.attributes.lookup(attrKey) must beSome("foo")
    }
  }
}
