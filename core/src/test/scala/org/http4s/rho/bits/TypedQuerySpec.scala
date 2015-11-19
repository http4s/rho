package org.http4s
package rho

import org.specs2.mutable.Specification

import UriTemplate.ParamExp

class TypedQuerySpec extends Specification {

  val request = Request()

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
      route.queryNames must equalTo(List("world"))
    }
    "find {?start}{&start}" in {
      val route = param[Int]("start", 0) && param[Int]("limit", 10)
      route.queryNames must equalTo(List("start", "limit"))
    }
    "find {?a}{&b}{&c}{&d}{&e}{&f}" in {
      val route = param[Int]("a") && param[Int]("b") && param[Int]("c") && param[Int]("d") && param[Int]("e")
      route.queryNames must equalTo(List("a", "b", "c", "d", "e"))
    }
  }

}
