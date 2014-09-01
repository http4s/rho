package org.http4s
package rho

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits.PathAST.PathRule
import org.http4s.rho.bits.QueryAST.QueryRule
import org.http4s.rho.bits.UriConverter

import shapeless.HList


/** A typed shell which represents the requirements of the route
 * @tparam T the HList representation of the values to be extracted from the [[Request]]
 */
trait TypedBuilder[T <: HList] extends UriConvertible {
  /** Untyped AST representation of the path to operate on */
  def path: PathRule

  /** Untyped AST representation of the query to operate on */
  def query: QueryRule

  /** Untyped AST representation of the [[Header]]s to operate on */
  def headers: HeaderRule

  override def asUriTemplate =
    for {
      p <- UriConverter.createPath(path)
      q <- UriConverter.createQuery(query)
    } yield UriTemplate(path = p, query = Some(q))
}
