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
