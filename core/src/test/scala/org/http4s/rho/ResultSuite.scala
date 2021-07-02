package org.http4s.rho

import cats.effect._
import cats.effect.unsafe.implicits.global
import munit.FunSuite
import org.http4s.headers._
import org.http4s.HttpDate
import org.http4s.rho.io._
import org.typelevel.vault._

class ResultSuite extends FunSuite {
  test("A ResultSyntax should add headers") {
    val date = Date(HttpDate.Epoch)
    val resp = Ok("Foo")
      .map(_.putHeaders(date))
      .unsafeRunSync()
      .resp

    assertEquals(resp.headers.get[Date], Some(date))

    val respNow = Ok("Foo")
      .map(_.putHeaders(date))
      .unsafeRunSync()
      .resp

    assertEquals(respNow.headers.get[Date], Some(date))
  }

  test("A ResultSyntax should add attributes") {
    val attrKey = Key.newKey[SyncIO, String].unsafeRunSync()
    val resp = Ok("Foo")
      .map(_.withAttribute(attrKey, "foo"))
      .unsafeRunSync()
      .resp

    assertEquals(resp.attributes.lookup(attrKey), Some("foo"))

    val resp2 = Ok("Foo")
      .map(_.withAttribute(attrKey, "foo"))
      .unsafeRunSync()
      .resp

    assertEquals(resp2.attributes.lookup(attrKey), Some("foo"))
  }
}
