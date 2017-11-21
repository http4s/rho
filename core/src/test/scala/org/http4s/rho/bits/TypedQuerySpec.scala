package org.http4s
package rho

import org.specs2.mutable.Specification
import UriTemplate.ParamExp
import UriTemplate.PathElm
import UriTemplate.PathExp
import cats.effect.IO

class TypedQuerySpec extends Specification {

  val request = Request[IO]()

  "TypedQuery.asUriTemplate" should {
    "convert to {?world}" in {
      val route = param[Int]("world")
      val q = List(ParamExp("world"))
      route.asUriTemplate(request).get must equalTo(UriTemplate(query = q))
    }
    "convert to {?start}{&start}" in {
      val route = param[Int]("start", 0) && param[Int]("limit", 10)
      val q = List(ParamExp("start"), ParamExp("limit"))
      route.asUriTemplate(request).get must equalTo(UriTemplate(query = q))
    }
  }

  "TypedQuery.names" should {
    "find {?world}" in {
      val route = param[Int]("world")
      route.names must equalTo(List("world"))
    }
    "find {?start}{&start}" in {
      val route = param[Int]("start", 0) && param[Int]("limit", 10)
      route.names must equalTo(List("start", "limit"))
    }
    "find {?a}{&b}{&c}{&d}{&e}{&f}" in {
      val route = param[Int]("a") && param[Int]("b") && param[Int]("c") && param[Int]("d") && param[Int]("e")
      route.names must equalTo(List("a", "b", "c", "d", "e"))
    }
  }

}
