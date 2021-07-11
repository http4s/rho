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
