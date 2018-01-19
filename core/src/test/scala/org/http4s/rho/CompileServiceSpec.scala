package org.http4s.rho

import cats.effect.IO
import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.io._
import org.http4s.{Method, Request, Uri}
import org.specs2.mutable.Specification

class CompileServiceSpec extends Specification {

  def getFoo(implicit c: CompileService[IO, _]): Unit = {
    GET / "hello" |>> "GetFoo"
  }

  def putFoo(implicit c: CompileService[IO, _]): Unit = {
    PUT / "hello" |>> "PutFoo"
  }

  "CompileService" should {
    "Build a single route" in {
      val c = ServiceBuilder[IO]()
      getFoo(c)

      "GetFoo" === RRunner(c.toService()).checkOk(Request(uri=Uri(path="/hello")))
    }

    "Build multiple routes" in {
      val c = ServiceBuilder[IO]()
      getFoo(c)
      putFoo(c)

      "GetFoo" === RRunner(c.toService()).checkOk(Request(uri=Uri(path="/hello")))
      "PutFoo" === RRunner(c.toService()).checkOk(Request(method = Method.PUT, uri=Uri(path="/hello")))
    }

    "Make routes from a collection of RhoRoutes" in {
      import CompileService.Implicit.compiler
      val routes =
        (GET / "hello" |>> "GetFoo") ::
        (PUT / "hello" |>> "PutFoo") :: Nil

      val srvc = CompileService.foldServices[IO](routes, identity)
      "GetFoo" === RRunner(srvc).checkOk(Request(uri=Uri(path="/hello")))
      "PutFoo" === RRunner(srvc).checkOk(Request(method = Method.PUT, uri=Uri(path="/hello")))
    }

    "Concatenate correctly" in {
      val c1 = ServiceBuilder[IO](); getFoo(c1)
      val c2 = ServiceBuilder[IO](); putFoo(c2)

      val srvc = c1.append(c2.routes()).toService()
      "GetFoo" === RRunner(srvc).checkOk(Request(uri=Uri(path="/hello")))
      "PutFoo" === RRunner(srvc).checkOk(Request(method = Method.PUT, uri=Uri(path="/hello")))
    }
  }

}
