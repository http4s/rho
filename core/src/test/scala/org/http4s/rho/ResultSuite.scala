package org.http4s.rho

import cats.effect._
import munit.CatsEffectSuite
import org.http4s.headers._
import org.http4s.HttpDate
import org.http4s.rho.io._
import org.typelevel.vault._

class ResultSuite extends CatsEffectSuite {
  test("A ResultSyntax should add headers") {
    val date = Date(HttpDate.Epoch)
    val resp = Ok("Foo")
      .map(_.putHeaders(date))
      .map(_.resp)

    assertIO(resp.map(_.headers.get[Date]), Some(date))

    val respNow = Ok("Foo")
      .map(_.putHeaders(date))
      .map(_.resp)

    assertIO(respNow.map(_.headers.get[Date]), Some(date))
  }

  test("A ResultSyntax should add attributes") {
    val attrKey = Key.newKey[SyncIO, String].unsafeRunSync()
    val resp = Ok("Foo")
      .map(_.withAttribute(attrKey, "foo"))
      .map(_.resp)

    assertIO(resp.map(_.attributes.lookup(attrKey)), Some("foo"))

    val resp2 = Ok("Foo")
      .map(_.withAttribute(attrKey, "foo"))
      .map(_.resp)

    assertIO(resp2.map(_.attributes.lookup(attrKey)), Some("foo"))
  }
}
