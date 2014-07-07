package org.http4s
package rho

import scalaz.Validation

/** Defines a type that is convertible into an `Uri` or `UriTemplate` */
trait UriConvertible {
  /**
   * Converts a route into an `Uri`.
   *
   * If the conversion fails `None` is returned. In case your route has
   * multiple paths only one way will be resolved as instance of `Uri`.
   * If the route is a URI Template but not an URI `None` will be returned.
   */
  //  def asUri: Uri
  /**
   * Converts a route into an `UriTemplate`.
   *
   * If the conversion fails `None` is returned. In case your route has
   * multiple paths only one way will be resolved as instance of `UriTemplate`.
   */
  def asUriTemplate: UriTemplate
}
