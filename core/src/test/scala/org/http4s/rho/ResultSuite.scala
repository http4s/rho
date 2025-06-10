/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

    val respNow = Ok("Foo")
      .map(_.putHeaders(date))
      .map(_.resp)

    assertIO(resp.map(_.headers.get[Date]), Some(date)) *>
      assertIO(respNow.map(_.headers.get[Date]), Some(date))
  }

  test("A ResultSyntax should add attributes") {
    val attrKey = Key.newKey[SyncIO, String].unsafeRunSync()
    val resp = Ok("Foo")
      .map(_.withAttribute(attrKey, "foo"))
      .map(_.resp)

    val resp2 = Ok("Foo")
      .map(_.withAttribute(attrKey, "foo"))
      .map(_.resp)

    assertIO(resp.map(_.attributes.lookup(attrKey)), Some("foo")) *>
      assertIO(resp2.map(_.attributes.lookup(attrKey)), Some("foo"))
  }
}
