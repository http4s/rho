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

package org.http4s
package rho
package bits

import cats.effect.IO
import munit.FunSuite
import org.http4s.UriTemplate.ParamExp
import org.http4s.rho.io._

class TypedQuerySuite extends FunSuite {
  val request: Request[IO] = Request[IO]()

  test("A TypedQuery.asUriTemplate should convert to {?world}") {
    val route = param[Int]("world")
    val q = List(ParamExp("world"))

    assertEquals(route.asUriTemplate(request).get, UriTemplate(query = q))
  }

  test("A TypedQuery.asUriTemplate should convert to {?start}{&start}") {
    val route = param[Int]("start", 0) && param[Int]("limit", 10)
    val q = List(ParamExp("start"), ParamExp("limit"))

    assertEquals(route.asUriTemplate(request).get, UriTemplate(query = q))
  }

  test("A TypedQuery.names should find {?world}") {
    val route = param[Int]("world")
    assertEquals(route.names, List("world"))
  }

  test("A TypedQuery.names should find {?start}{&start}") {
    val route = param[Int]("start", 0) && param[Int]("limit", 10)
    assertEquals(route.names, List("start", "limit"))
  }

  test("A TypedQuery.names should find {?a}{&b}{&c}{&d}{&e}{&f}") {
    val route =
      param[Int]("a") && param[Int]("b") && param[Int]("c") && param[Int]("d") && param[Int]("e")
    assertEquals(route.names, List("a", "b", "c", "d", "e"))
  }
}
