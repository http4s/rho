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

import scala.util.Try

/** Defines a type that is convertible into an `Uri` or `UriTemplate` */
trait UriConvertible[F[_]] {

  /** Converts a route into an `Uri`.
    *
    * If the conversion fails `None` is returned. In case your route has
    * multiple paths only one way will be resolved as instance of `Uri`.
    * If the route is a URI Template but not an URI `None` will be returned.
    */
  def asUri(request: Request[F]): Try[Uri] =
    for {
      t <- asUriTemplate(request)
      u <- t.toUriIfPossible
    } yield u

  /** Converts a route into an `UriTemplate`.
    *
    * If the conversion fails `None` is returned. In case your route has
    * multiple paths only one way will be resolved as instance of `UriTemplate`.
    */
  def asUriTemplate(request: Request[F]): Try[UriTemplate]

}

object UriConvertible {

  private[rho] def respectPathInfo[F[_]](
      uriTemplate: Try[UriTemplate],
      request: Request[F]): Try[UriTemplate] =
    for (tpl <- uriTemplate)
      yield UriConvertible.addPathInfo(request, tpl)

  private[rho] def addPathInfo[F[_]](request: Request[F], tpl: UriTemplate): UriTemplate = {
    val caret = request.attributes.lookup(Request.Keys.PathInfoCaret).getOrElse(0)
    if (caret == 0) tpl
    else if (caret == 1 && request.scriptName.absolute) tpl
    else
      tpl.copy(path = UriTemplate.PathElm(request.scriptName.toRelative.renderString) :: tpl.path)
  }
}
