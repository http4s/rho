package org.http4s
package rho

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** Defines a type that is convertible into an `Uri` or `UriTemplate` */
trait UriConvertible {

  /**
   * Converts a route into an `Uri`.
   *
   * If the conversion fails `None` is returned. In case your route has
   * multiple paths only one way will be resolved as instance of `Uri`.
   * If the route is a URI Template but not an URI `None` will be returned.
   */
  def asUri: Try[Uri] =
    for {
      t <- asUriTemplate
      u <- t.toUriIfPossible
    } yield { u }

  /**
   * Converts a route into an `UriTemplate`.
   *
   * If the conversion fails `None` is returned. In case your route has
   * multiple paths only one way will be resolved as instance of `UriTemplate`.
   */
  def asUriTemplate: Try[UriTemplate]

}
