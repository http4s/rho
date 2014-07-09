package org.http4s
package rho

import org.http4s.rho.bits.QueryAST.{ QueryAnd, TypedQuery, QueryRule }
import org.http4s.rho.bits.UriConverter

import bits.HeaderAST.{ EmptyHeaderRule, HeaderRule }
import bits.PathAST.PathRule
import shapeless.HList
import shapeless.ops.hlist.Prepend

case class RequestLineBuilder[T <: HList](path: PathRule, query: QueryRule)
  extends TypedBuilder[T]
  with UriConvertible {

  override def validators: HeaderRule = EmptyHeaderRule

  def &[T1 <: HList](q: TypedQuery[T1])(implicit prep: Prepend[T1, T]): RequestLineBuilder[prep.Out] =
    RequestLineBuilder(path, QueryAnd(query, q.rule))

  override def asUriTemplate =
    for {
      p <- UriConverter.createPath(path)
      q <- UriConverter.createQuery(query)
    } yield UriTemplate(path = p, query = Some(q))

}
