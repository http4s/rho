/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.rho

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.Method
import org.http4s.Request
import org.http4s.rho.bits.MethodAliases._
import org.http4s.rho.io._

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

    assertIO(RRunner(c.toRoutes()).checkOk(Request(uri = uri"/hello")), "GetFoo") *>
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
    assertIO(RRunner(srvc).checkOk(Request(uri = uri"/hello")), "GetFoo") *>
      assertIO(RRunner(srvc).checkOk(Request(method = Method.PUT, uri = uri"/hello")), "PutFoo")
  }

  test("A CompileService should concatenate correctly") {
    val c1 = RoutesBuilder[IO](); getFoo(c1)
    val c2 = RoutesBuilder[IO](); putFoo(c2)

    val srvc = c1.append(c2.routes()).toRoutes()

    assertIO(RRunner(srvc).checkOk(Request(uri = uri"/hello")), "GetFoo") *>
      assertIO(RRunner(srvc).checkOk(Request(method = Method.PUT, uri = uri"/hello")), "PutFoo")
  }
}
