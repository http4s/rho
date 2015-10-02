package org.http4s.rho

import org.http4s.{Method, Request, Uri}
import org.http4s.rho.CompileService.ServiceBuilder
import org.specs2.mutable.Specification

class CompileServiceSpec extends Specification {

  def getFoo(implicit c: CompileService[_]): Unit = {
    import c.dsl._
    GET / "hello" |>> "GetFoo"
  }

  def putFoo(implicit c: CompileService[_]): Unit = {
    import c.dsl._
    PUT / "hello" |>> "PutFoo"
  }

  "CompileService" should {
    "Build a single route" in {
      val c = ServiceBuilder()
      getFoo(c)

      "GetFoo" === new RRunner(c.toService()).checkOk(Request(uri=Uri(path="/hello")))
    }

    "Build multiple routes" in {
      val c = ServiceBuilder()
      getFoo(c)
      putFoo(c)

      "GetFoo" === new RRunner(c.toService()).checkOk(Request(uri=Uri(path="/hello")))
      "PutFoo" === new RRunner(c.toService()).checkOk(Request(method = Method.PUT, uri=Uri(path="/hello")))
    }
  }

}
