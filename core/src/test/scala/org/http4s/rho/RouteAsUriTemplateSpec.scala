package org.http4s
package rho

import org.specs2.mutable.Specification

import UriTemplate.ParamExp
import UriTemplate.PathElm
import UriTemplate.PathExp
import cats.effect.IO

import bits.MethodAliases.GET

class RouteAsUriTemplateSpec extends Specification {

  val request = Request[IO]()

  "PathBuilder.asUriTemplate" should {
    "convert to /hello" in {
      val route = GET / "hello"
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"))))
    }
    "convert to /hello/world" in {
      val route = GET / "hello" / "world"
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"), PathElm("world"))))
    }
    "convert to /hello{/world}" in {
      val route = GET / "hello" / 'world
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"), PathExp("world"))))
    }
    "convert to /hello/world/next/time" in {
      val route1 = "hello" / "world"
      val route2 = "next" / "time"
      val route = GET / route1 / route2
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"), PathElm("world"), PathElm("next"), PathElm("time"))))
    }
    "convert to {/id}" in {
      val route = GET / pathVar[Int]("id")
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathExp("id"))))
    }
    "convert pathVar[Int] to {/int}" in {
      val route = GET / pathVar[Int]
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathExp("int"))))
      true
    }
    "convert to /orders{/id}/items" in {
      val route = GET / "orders" / pathVar[Int]("id") / "items"
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("orders"), PathExp("id"), PathElm("items"))))
    }
  }

  "QueryBuilder.asUriTemplate" should {
    "convert to /hello{?world}" in {
      val route = GET / "hello" +? param[Int]("world")
      val p = List(PathElm("hello"))
      val q = List(ParamExp("world"))
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = p, query = q))
    }
    "convert to /hello/world{?start}{&start}" in {
      val route = GET / "hello" / "world" +? param[Int]("start") & param[Int]("limit")
      val p = List(PathElm("hello"), PathElm("world"))
      val q = List(ParamExp("start"), ParamExp("limit"))
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = p, query = q))
    }
  }

  "RequestLineBuilder.asUriTemplate" should {
    "convert to /hello{/world}" in {
      val requestLine = "hello" / pathVar[String]("world")
      val p = List(PathElm("hello"), PathExp("world"))
      requestLine.asUriTemplate(request).get must equalTo(UriTemplate(path = p))
    }
    "convert to /hello{/world}/test" in {
      val requestLine = "hello" / pathVar[String]("world") / "user"
      val p = List(PathElm("hello"), PathExp("world"), PathElm("user"))
      requestLine.asUriTemplate(request).get must equalTo(UriTemplate(path = p))
    }
    "convert to /hello{?world}" in {
      val requestLine = "hello" +? param[Int]("world")
      val p = List(PathElm("hello"))
      val q = List(ParamExp("world"))
      requestLine.asUriTemplate(request).get must equalTo(UriTemplate(path = p, query = q))
    }
    "convert to /hello/world{?start}{&limit}" in {
      val requestLine = "hello" / "world" +? param[Int]("start") & param[Int]("limit")
      val p = List(PathElm("hello"), PathElm("world"))
      val q = List(ParamExp("start"), ParamExp("limit"))
      requestLine.asUriTemplate(request).get must equalTo(UriTemplate(path = p, query = q))
    }
    "convert to /hello{/world}{?start}{&limit}" in {
      val requestLine = "hello" / pathVar[String]("world") +? param[Int]("start") & param[Int]("limit")
      val p = List(PathElm("hello"), PathExp("world"))
      val q = List(ParamExp("start"), ParamExp("limit"))
      requestLine.asUriTemplate(request).get must equalTo(UriTemplate(path = p, query = q))
    }
  }

  "TypedPath.asUriTemplate" should {
    "convert to /hello" in {
      val route = GET / "hello"
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"))))
    }
    "convert to /hello/world" in {
      val route = "hello" / "world"
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"), PathElm("world"))))
    }
    "convert to /hello{/world}" in {
      val route = "hello" / 'world
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"), PathExp("world"))))
    }
    "convert to /hello/world/next/time" in {
      val route1 = "hello" / "world"
      val route2 = "next" / "time"
      val route = route1 && route2
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("hello"), PathElm("world"), PathElm("next"), PathElm("time"))))
    }
    "convert to {/id}" in {
      val route = pathVar[Int]("id")
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathExp("id"))))
    }
    "convert pathVar[Int] to {/int}" in {
      val route = pathVar[Int]
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathExp("int"))))
      true
    }
    "convert to /orders{/id}/items" in {
      val route = "orders" / pathVar[Int]("id") / "items"
      route.asUriTemplate(request).get must equalTo(UriTemplate(path = List(PathElm("orders"), PathExp("id"), PathElm("items"))))
    }
  }

}
