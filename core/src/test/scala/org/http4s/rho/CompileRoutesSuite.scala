package org.http4s.rho

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.io._
import org.http4s.{Method, Request}

class CompileRoutesSuite extends CatsEffectSuite {
  private def getFoo(implicit c: CompileRoutes[IO, _]) =
    GET / "hello" |>> "GetFoo"

  private def putFoo(implicit c: CompileRoutes[IO, _]) =
    PUT / "hello" |>> "PutFoo"

  test("A CompileService should build a single route") {
    val c = RoutesBuilder[IO]()
    getFoo(c)

    assertIO(RRunner(c.toRoutes()).checkOk(Request(uri = uri"/hello")), "GetFoo")
  }

  test("A CompileService should build multiple routes") {
    val c = RoutesBuilder[IO]()
    getFoo(c)
    putFoo(c)

    assertIO(RRunner(c.toRoutes()).checkOk(Request(uri = uri"/hello")), "GetFoo")
    assertIO(
      RRunner(c.toRoutes()).checkOk(Request(method = Method.PUT, uri = uri"/hello")),
      "PutFoo"
    )
  }

  test("A CompileService should make routes from a collection of RhoRoutes") {
    import CompileRoutes.Implicit.compiler
    val routes =
      (GET / "hello" |>> "GetFoo") ::
        (PUT / "hello" |>> "PutFoo") :: Nil

    val srvc = CompileRoutes.foldRoutes[IO](routes)
    assertIO(RRunner(srvc).checkOk(Request(uri = uri"/hello")), "GetFoo")
    assertIO(RRunner(srvc).checkOk(Request(method = Method.PUT, uri = uri"/hello")), "PutFoo")
  }

  test("A CompileService should concatenate correctly") {
    val c1 = RoutesBuilder[IO](); getFoo(c1)
    val c2 = RoutesBuilder[IO](); putFoo(c2)

    val srvc = c1.append(c2.routes()).toRoutes()
    assertIO(RRunner(srvc).checkOk(Request(uri = uri"/hello")), "GetFoo")
    assertIO(RRunner(srvc).checkOk(Request(method = Method.PUT, uri = uri"/hello")), "PutFoo")
  }
}
