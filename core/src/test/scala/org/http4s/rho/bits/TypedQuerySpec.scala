package org.http4s
package rho

import cats.effect.IO
import org.http4s.UriTemplate.ParamExp
import org.specs2.mutable.Specification

class TypedQuerySpec extends Specification {
  val request: Request[IO] = Request[IO]()

  "TypedQuery.asUriTemplate" should {
    "convert to {?world}" in {
      val route = param[IO, Int]("world")
      val q = List(ParamExp("world"))

      route.asUriTemplate(request).get must equalTo(UriTemplate(query = q))
    }

    "convert to {?start}{&start}" in {
      val route = param[IO, Int]("start", 0) && param[IO, Int]("limit", 10)
      val q = List(ParamExp("start"), ParamExp("limit"))

      route.asUriTemplate(request).get must equalTo(UriTemplate(query = q))
    }
  }

  "TypedQuery.names" should {
    "find {?world}" in {
      val route = param[IO, Int]("world")
      route.names must equalTo(List("world"))
    }

    "find {?start}{&start}" in {
      val route = param[IO, Int]("start", 0) && param[IO, Int]("limit", 10)
      route.names must equalTo(List("start", "limit"))
    }

    "find {?a}{&b}{&c}{&d}{&e}{&f}" in {
      val route = param[IO, Int]("a") && param[IO, Int]("b") && param[IO, Int]("c") && param[IO, Int]("d") && param[IO, Int]("e")
      route.names must equalTo(List("a", "b", "c", "d", "e"))
    }
  }
}
