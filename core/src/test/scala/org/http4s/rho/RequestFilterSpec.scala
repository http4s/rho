package org.http4s
package rho

import bits.MethodAliases._
import bits.ResponseGeneratorInstances._

import org.specs2.mutable.Specification

import scalaz.concurrent.Task

/**
 * Created on 7/9/15.
 */
class RequestFilterSpec extends Specification {

  "BeforeFilters" should {

    val path = GET / "foo"
    val req = Request(uri = uri("/foo"))

    "Let a response pass through" in {
      val passFilter = RequestFilter.pureFilter(_ => None: Option[String])
      val route = path.runWith(passFilter ^?> { () => Ok("Good.")})
      route(req).run.map(_.status) must beSome(Status.Ok)
    }

    "Let a response pass through asynchronously" in {
      val passFilter = RequestFilter.filter(_ => Task(None: Option[String]))
      val route = path.runWith(passFilter ^?> { () => Ok("Good.")})
      route(req).run.map(_.status) must beSome(Status.Ok)
    }

    "Chose the filtered response" in {
      val passFilter = RequestFilter.pureFilter(_ => Some(Unauthorized("Boo..")))
      val route = path.runWith(passFilter ^?> { () => sys.error("Shouldn't get here!!!"); Ok("Good.") })
      route(req).run.map(_.status) must beSome(Status.Unauthorized)
    }

    "Chose the filtered response asynchronously" in {
      val passFilter = RequestFilter.filter(_ => Task(Some(Unauthorized("Boo.."))))
      val route = path.runWith(passFilter ^?> { () => sys.error("Shouldn't get here!!!"); Ok("Good.") })
      route(req).run.map(_.status) must beSome(Status.Unauthorized)
    }
  }
}
