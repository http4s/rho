package org.http4s
package rho

import cats.effect.IO
import munit.FunSuite
import org.typelevel.vault._
import UriTemplate._

import scala.util.Success
import scala.util.Failure

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
