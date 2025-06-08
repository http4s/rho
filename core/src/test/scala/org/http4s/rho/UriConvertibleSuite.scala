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

import cats.effect.IO
import munit.FunSuite
import org.typelevel.vault._

import scala.util.Failure
import scala.util.Success

import UriTemplate._

class UriConvertibleSuite extends FunSuite {
  test("An UriConvertible.respectPathInfo should respect if URI template is available") {
    val request = Request[IO](
      uri = uri"/some",
      attributes = Vault.empty.insert(Request.Keys.PathInfoCaret, 5)
    )
    val path = List(PathElm("here"))
    val query = List(ParamVarExp("ref", "path"))
    val tpl = UriTemplate(path = path, query = query)

    assertEquals(
      UriConvertible.respectPathInfo(Success(tpl), request).get.toString,
      "/some/here?ref={path}"
    )
  }

  test("An UriConvertible.respectPathInfo should do nothing if URI template is not available") {
    val request = Request[IO]()
    assert(
      UriConvertible
        .respectPathInfo(Failure(new Exception("URI not available")), request)
        .isFailure
    )
  }

  test("An UriConvertible.addPathInfo should keep the path if PathInfoCaret is not available") {
    val request = Request[IO](uri = uri"/some")
    val path = List(PathElm("here"))
    val query = List(ParamVarExp("ref", "path"))
    val tpl = UriTemplate(path = path, query = query)

    assertEquals(UriConvertible.addPathInfo(request, tpl).toString, "/here?ref={path}")
  }

  test("An UriConvertible.addPathInfo should keep the path if PathInfoCaret is 0") {
    val request = Request[IO](
      uri = uri"/some",
      attributes = Vault.empty.insert(Request.Keys.PathInfoCaret, 0)
    )
    val path = List(PathElm("here"))
    val query = List(ParamVarExp("ref", "path"))
    val tpl = UriTemplate(path = path, query = query)

    assertEquals(UriConvertible.addPathInfo(request, tpl).toString, "/here?ref={path}")
  }

  test("An UriConvertible.addPathInfo should keep the path if PathInfoCaret is 1") {
    val request = Request[IO](
      uri = uri"/some",
      attributes = Vault.empty.insert(Request.Keys.PathInfoCaret, 1)
    )
    val path = List(PathElm("here"))
    val query = List(ParamVarExp("ref", "path"))
    val tpl = UriTemplate(path = path, query = query)

    assertEquals(UriConvertible.addPathInfo(request, tpl).toString, "/here?ref={path}")
  }

  test("An UriConvertible.addPathInfo should manipulate the path if PathInfoCaret greater than 1") {
    val request = Request[IO](
      uri = uri"/some",
      attributes = Vault.empty.insert(Request.Keys.PathInfoCaret, 5)
    )
    val path = List(PathElm("here"))
    val query = List(ParamVarExp("ref", "path"))
    val tpl = UriTemplate(path = path, query = query)

    assertEquals(UriConvertible.addPathInfo(request, tpl).toString, "/some/here?ref={path}")
  }
}
