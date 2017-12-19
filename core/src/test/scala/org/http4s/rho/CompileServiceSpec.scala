package org.http4s.rho

import cats.effect.IO
import org.http4s.{Method, Request, RhoDsl, Uri, rho}
import org.specs2.mutable.Specification
import rho.bits.MethodAliases._

import scala.language.existentials

class CompileServiceSpec extends Specification {
  val rhoDsl: RhoDsl[IO] = rho.apply[IO]
  import rhoDsl._

  import CompileService.Implicit.compiler

  def getFoo[F[_]](implicit c: CompileService[F, _]): Unit = {
    GET / "hello" |>> "GetFoo"
  }

  def putFoo[F[_]](implicit c: CompileService[F, _]): Unit = {
    PUT / "hello" |>> "PutFoo"
  }

  "CompileService" should {
    "Build a single route" in {
      val c = ServiceBuilder[IO]()
      getFoo(c)

      "GetFoo" === new RRunner(c.toService()).checkOk(Request(uri=Uri(path="/hello")))
    }

    "Build multiple routes" in {
      val c = ServiceBuilder[IO]()
      getFoo(c)
      putFoo(c)

      "GetFoo" === new RRunner(c.toService()).checkOk(Request(uri=Uri(path="/hello")))
      "PutFoo" === new RRunner(c.toService()).checkOk(Request(method = Method.PUT, uri=Uri(path="/hello")))
    }

    "Make routes from a collection of RhoRoutes" in {
      val routes =
        (GET / "hello" |>> "GetFoo") ::
        (PUT / "hello" |>> "PutFoo") :: Nil

      val srvc = CompileService.foldServices[IO](routes, identity)
      "GetFoo" === new RRunner(srvc).checkOk(Request(uri=Uri(path="/hello")))
      "PutFoo" === new RRunner(srvc).checkOk(Request(method = Method.PUT, uri=Uri(path="/hello")))
    }

    "Concatenate correctly" in {
      val c1 = ServiceBuilder[IO](); getFoo(c1)
      val c2 = ServiceBuilder[IO](); putFoo(c2)

      val srvc = c1.append(c2.routes()).toService()
      "GetFoo" === new RRunner(srvc).checkOk(Request(uri=Uri(path="/hello")))
      "PutFoo" === new RRunner(srvc).checkOk(Request(method = Method.PUT, uri=Uri(path="/hello")))
    }
  }

}
