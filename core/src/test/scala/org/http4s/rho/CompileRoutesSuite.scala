package org.http4s.rho

import cats.effect.IO
import munit.FunSuite
import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.io._
import org.http4s.{Method, Request}

class CompileRoutesSuite extends FunSuite {

  def getFoo(implicit c: CompileRoutes[IO, _]) =
    GET / "hello" |>> "GetFoo"

  def putFoo(implicit c: CompileRoutes[IO, _]) =
    PUT / "hello" |>> "PutFoo"

  test("A CompileService should build a single route") {
    val c = RoutesBuilder[IO]()
    getFoo(c)

    assertEquals("GetFoo", RRunner(c.toRoutes()).checkOk(Request(uri = uri"/hello")))
  }

  test("A CompileService should build multiple routes") {
    val c = RoutesBuilder[IO]()
    getFoo(c)
    putFoo(c)

    assertEquals("GetFoo", RRunner(c.toRoutes()).checkOk(Request(uri = uri"/hello")))
    assertEquals(
      "PutFoo",
      RRunner(c.toRoutes()).checkOk(Request(method = Method.PUT, uri = uri"/hello"))
    )
  }

  test("A CompileService should make routes from a collection of RhoRoutes") {
    import CompileRoutes.Implicit.compiler
    val routes =
      (GET / "hello" |>> "GetFoo") ::
        (PUT / "hello" |>> "PutFoo") :: Nil

    val srvc = CompileRoutes.foldRoutes[IO](routes)
    assertEquals("GetFoo", RRunner(srvc).checkOk(Request(uri = uri"/hello")))
    assertEquals("PutFoo", RRunner(srvc).checkOk(Request(method = Method.PUT, uri = uri"/hello")))
  }

  test("A CompileService should concatenate correctly") {
    val c1 = RoutesBuilder[IO](); getFoo(c1)
    val c2 = RoutesBuilder[IO](); putFoo(c2)

    val srvc = c1.append(c2.routes()).toRoutes()
    assertEquals("GetFoo", RRunner(srvc).checkOk(Request(uri = uri"/hello")))
    assertEquals("PutFoo", RRunner(srvc).checkOk(Request(method = Method.PUT, uri = uri"/hello")))
  }
}
