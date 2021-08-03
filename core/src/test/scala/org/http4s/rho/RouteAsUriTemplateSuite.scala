package org.http4s
package rho

import cats.effect.IO
import munit.FunSuite
import org.http4s.rho.bits.MethodAliases.GET
import org.http4s.rho.io._
import org.http4s.UriTemplate.{ParamExp, PathElm, PathExp}
import shapeless.HList

class RouteAsUriTemplateSuite extends FunSuite {
  val request = Request[IO]()

  test("A PathBuilder.asUriTemplate should convert to /hello") {
    val route: PathBuilder[IO, _ <: HList] = GET / "hello"
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = List(PathElm("hello"))))
  }

  test("A PathBuilder.asUriTemplate should convert to /hello/world") {
    val route: PathBuilder[IO, _ <: HList] = GET / "hello" / "world"
    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("hello"), PathElm("world")))
    )
  }

  test("A PathBuilder.asUriTemplate should convert to /hello{/world}") {
    val route: PathBuilder[IO, _ <: HList] = GET / "hello" / pv"world"
    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("hello"), PathExp("world")))
    )
  }

  test("A PathBuilder.asUriTemplate should convert to /hello/world/next/time") {
    val route1 = "hello" / "world"
    val route2 = "next" / "time"
    val route = GET / route1 / route2

    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("hello"), PathElm("world"), PathElm("next"), PathElm("time")))
    )
  }

  test("A PathBuilder.asUriTemplate should convert to {/id}") {
    val route: PathBuilder[IO, _ <: HList] = GET / pathVar[Int]("id")
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = List(PathExp("id"))))
  }

  test("A PathBuilder.asUriTemplate should convert pathVar[Int] to {/int}") {
    val route: PathBuilder[IO, _ <: HList] = GET / pathVar[Int]
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = List(PathExp("int"))))
  }

  test("A PathBuilder.asUriTemplate should convert to /orders{/id}/items") {
    val route: PathBuilder[IO, _ <: HList] = GET / "orders" / pathVar[Int]("id") / "items"
    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("orders"), PathExp("id"), PathElm("items")))
    )
  }

  test("A QueryBuilder.asUriTemplate should convert to /hello{?world}") {
    val route = GET / "hello" +? param[Int]("world")
    val p = List(PathElm("hello"))
    val q = List(ParamExp("world"))
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = p, query = q))
  }

  test("A QueryBuilder.asUriTemplate should convert to /hello/world{?start}{&start}") {
    val route = GET / "hello" / "world" +? param[Int]("start") & param[Int]("limit")
    val p = List(PathElm("hello"), PathElm("world"))
    val q = List(ParamExp("start"), ParamExp("limit"))
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = p, query = q))
  }

  test("A RequestLineBuilder.asUriTemplate should convert to /hello{/world}") {
    val requestLine = "hello" / pathVar[String]("world")
    val p = List(PathElm("hello"), PathExp("world"))
    assertEquals(requestLine.asUriTemplate(request).get, UriTemplate(path = p))
  }

  test("A RequestLineBuilder.asUriTemplate should convert to /hello{/world}/test") {
    val requestLine = "hello" / pathVar[String]("world") / "user"
    val p = List(PathElm("hello"), PathExp("world"), PathElm("user"))
    assertEquals(requestLine.asUriTemplate(request).get, UriTemplate(path = p))
  }

  test("A RequestLineBuilder.asUriTemplate should convert to /hello{?world}") {
    val requestLine = "hello" +? param[Int]("world")
    val p = List(PathElm("hello"))
    val q = List(ParamExp("world"))
    assertEquals(requestLine.asUriTemplate(request).get, UriTemplate(path = p, query = q))
  }

  test("A RequestLineBuilder.asUriTemplate should convert to /hello/world{?start}{&limit}") {
    val requestLine = "hello" / "world" +? param[Int]("start") & param[Int]("limit")
    val p = List(PathElm("hello"), PathElm("world"))
    val q = List(ParamExp("start"), ParamExp("limit"))
    assertEquals(requestLine.asUriTemplate(request).get, UriTemplate(path = p, query = q))
  }

  test("A RequestLineBuilder.asUriTemplate should convert to /hello{/world}{?start}{&limit}") {
    val requestLine =
      "hello" / pathVar[String]("world") +? param[Int]("start") & param[Int]("limit")
    val p = List(PathElm("hello"), PathExp("world"))
    val q = List(ParamExp("start"), ParamExp("limit"))
    assertEquals(requestLine.asUriTemplate(request).get, UriTemplate(path = p, query = q))
  }

  test("A TypedPath.asUriTemplate should convert to /hello") {
    val route = GET / "hello"
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = List(PathElm("hello"))))
  }

  test("A TypedPath.asUriTemplate should convert to /hello/world") {
    val route = "hello" / "world"
    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("hello"), PathElm("world")))
    )
  }

  test("A TypedPath.asUriTemplate should convert to /hello{/world}") {
    val route = "hello" / pv"world"
    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("hello"), PathExp("world")))
    )
  }

  test("A TypedPath.asUriTemplate should convert to /hello/world/next/time") {
    val route1 = "hello" / "world"
    val route2 = "next" / "time"
    val route = route1 && route2

    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("hello"), PathElm("world"), PathElm("next"), PathElm("time")))
    )
  }

  test("A TypedPath.asUriTemplate should convert to {/id}") {
    val route = pathVar[Int]("id")
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = List(PathExp("id"))))
  }

  test("A TypedPath.asUriTemplate should convert pathVar[Int] to {/int}") {
    val route = pathVar[Int]
    assertEquals(route.asUriTemplate(request).get, UriTemplate(path = List(PathExp("int"))))
  }

  test("A TypedPath.asUriTemplate should convert to /orders{/id}/items") {
    val route = "orders" / pathVar[Int]("id") / "items"
    assertEquals(
      route.asUriTemplate(request).get,
      UriTemplate(path = List(PathElm("orders"), PathExp("id"), PathElm("items")))
    )
  }
}
