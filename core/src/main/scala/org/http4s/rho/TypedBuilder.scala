package org.http4s
package rho

import org.http4s.rho.bits.PathAST.PathRule
import org.http4s.rho.bits.RequestAST.RequestRule
import org.http4s.rho.bits.UriConverter

import shapeless.HList


/** A typed shell which represents the requirements of the route
 * @tparam T the HList representation of the values to be extracted from the [[Request]]
 */
trait TypedBuilder[T <: HList] extends UriConvertible {
  /** Untyped AST representation of the path to operate on */
  val path: PathRule

  /** Untyped AST describing the extraction of headers and the query from the [[Request]] */
  val rules: RequestRule

  private def uriTemplate =
    for {
      p <- UriConverter.createPath(path)
      q <- UriConverter.createQuery(rules)
    } yield UriTemplate(path = p, query = q)

  final override def asUriTemplate(request: Request) =
    UriConvertible.respectPathInfo(uriTemplate, request)
}
