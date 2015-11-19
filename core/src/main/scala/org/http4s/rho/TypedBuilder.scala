package org.http4s
package rho

import org.http4s.rho.bits.PathAST.PathRule
import org.http4s.rho.bits.RequestRuleAST.RequestRule
import org.http4s.rho.bits.UriConverter

import shapeless.HList


/** A typed shell which represents the requirements of the route
 * @tparam T the HList representation of the values to be extracted from the [[Request]]
 */
trait TypedBuilder[T <: HList] extends UriConvertible {
  /** Untyped AST representation of the path to operate on */
  def path: PathRule

  /** Untyped AST representation of the query to operate on */
  def requestRules: RequestRule

  private val uriTemplate =
    for {
      p <- UriConverter.createPath(path)
      q <- UriConverter.createQuery(requestRules)
    } yield UriTemplate(path = p, query = q)

  final override def asUriTemplate(request: Request) =
    UriConvertible.respectPathInfo(uriTemplate, request)
}
