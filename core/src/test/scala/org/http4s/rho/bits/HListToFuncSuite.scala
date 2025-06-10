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

package org.http4s
package rho
package bits

import cats.effect.IO
import munit.CatsEffectSuite

class HListToFuncSuite extends CatsEffectSuite {
  private def getBody(b: EntityBody[IO]): IO[String] =
    b.compile.toVector.map(_.foldLeft(Array[Byte]())(_ :+ _)).map(new String(_))

  private def checkOk(r: Request[IO]): IO[String] =
    service(r).value.map(_.getOrElse(Response.notFound).body).flatMap(getBody)

  private def Get(s: String, h: Header.ToRaw*): Request[IO] =
    Request(
      bits.MethodAliases.GET,
      Uri.fromString(s).getOrElse(sys.error("Failed.")),
      headers = Headers(h: _*)
    )

  private val service = new RhoRoutes[IO] {
    GET / "route1" |>> { () => Ok("foo") }
  }.toRoutes()

  test("An HListToFunc should work for methods of type _ => Task[Response]") {
    val req = Get("/route1")
    assertIO(checkOk(req), "foo")
  }

  // Tests issue 218 https://github.com/http4s/rho/issues/218
  test("An HListToFunc should work with cats.implicits") {
    import cats.implicits._
    new RhoRoutes[IO] {
      // `.pure[IO]` used to require the cats.implicits under test
      GET / "route1" |>> { () => "foo".pure[IO].flatMap(Ok(_)) }
    }
  }
}
