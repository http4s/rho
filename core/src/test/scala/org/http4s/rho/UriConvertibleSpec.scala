package org.http4s
package rho

import org.specs2.mutable.Specification
import UriTemplate._
import cats.effect.IO

import scala.util.Success
import scala.util.Failure

object UriConvertibleSpec extends Specification {

  "UriConvertible.respectPathInfo" should {
    "respect if URI template is available" in {
      val request = Request[IO](
        uri = Uri(path = "/some"),
        attributes = AttributeMap(AttributeEntry(Request.Keys.PathInfoCaret, 5)))
      val path = List(PathElm("here"))
      val query = List(ParamVarExp("ref", "path"))
      val tpl = UriTemplate(path = path, query = query)

      UriConvertible.respectPathInfo(Success(tpl), request).get.toString must equalTo("/some/here?ref={path}")
    }
    "do nothing if URI template is not available" in {
      val request = Request[IO]()
      UriConvertible.respectPathInfo(Failure(new Exception("URI not available")), request).isFailure must_== true
    }
  }

  "UriConvertible.addPathInfo" should {
    "keep the path if PathInfoCaret is not available" in {
      val request = Request[IO](uri = Uri(path = "/some"))
      val path = List(PathElm("here"))
      val query = List(ParamVarExp("ref", "path"))
      val tpl = UriTemplate(path = path, query = query)

      UriConvertible.addPathInfo(request, tpl).toString must equalTo("/here?ref={path}")
    }
    "keep the path if PathInfoCaret is 0" in {
      val request = Request[IO](
        uri = Uri(path = "/some"),
        attributes = AttributeMap(AttributeEntry(Request.Keys.PathInfoCaret, 0)))
      val path = List(PathElm("here"))
      val query = List(ParamVarExp("ref", "path"))
      val tpl = UriTemplate(path = path, query = query)

      UriConvertible.addPathInfo(request, tpl).toString must equalTo("/here?ref={path}")
    }
    "keep the path if PathInfoCaret is 1" in {
      val request = Request[IO](
        uri = Uri(path = "/some"),
        attributes = AttributeMap(AttributeEntry(Request.Keys.PathInfoCaret, 1)))
      val path = List(PathElm("here"))
      val query = List(ParamVarExp("ref", "path"))
      val tpl = UriTemplate(path = path, query = query)

      UriConvertible.addPathInfo(request, tpl).toString must equalTo("/here?ref={path}")
    }
    "manipulate the path if PathInfoCaret greater than 1" in {
      val request = Request[IO](
        uri = Uri(path = "/some"),
        attributes = AttributeMap(AttributeEntry(Request.Keys.PathInfoCaret, 5)))
      val path = List(PathElm("here"))
      val query = List(ParamVarExp("ref", "path"))
      val tpl = UriTemplate(path = path, query = query)

      UriConvertible.addPathInfo(request, tpl).toString must equalTo("/some/here?ref={path}")
    }
  }

}
