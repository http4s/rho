package org.http4s.rho.bits

import org.http4s.UriTemplate.Path
import org.http4s.UriTemplate.Query

import PathAST.PathRule
import QueryAST.QueryRule

import scala.util.Success
import scala.util.Try

/** Helps to convert different parts of a route into an `UriTemplate` */
object UriConverter {

  def createPath(rule: PathRule): Try[Path] = {
    import PathAST._
    import org.http4s.UriTemplate.PathElm
    import org.http4s.UriTemplate.PathExp
    @scala.annotation.tailrec
    def go(r: List[PathRule], acc: Path): Path = r match {
      case Nil                         => acc
      case PathAnd(a, b) :: rs         => go(a :: b :: rs, acc)
      case PathOr(a, _) :: rs          => go(a :: rs, acc) // we decided to take the first root
      case PathMatch("") :: rs         => go(rs, acc)
      case PathMatch(s) :: rs          => go(rs, PathElm(s) :: acc)
      case PathCapture(id, _, _) :: rs => go(rs, PathExp(id) :: acc)
      case CaptureTail :: rs         => go(rs, acc)
      case MetaCons(p, _) :: rs        => go(p::rs, acc)
    }
    Success(go(List(rule), Nil).reverse)
  }

  def createQuery(rule: QueryRule): Try[Query] = {
    import QueryAST._
    import org.http4s.UriTemplate.ParamExp
    @scala.annotation.tailrec
    def go(r: List[QueryRule], acc: Query): Query = r match {
      case Nil                               => acc
      case MetaCons(query, _) :: rs          => go(rs, acc)
      case QueryAnd(a, b) :: rs              => go(a :: b :: rs, acc)
      case QueryCapture(reader) :: rs        => go(rs, reader.parameters.map(p => ParamExp(p.name)).toList ::: acc)
      case QueryOr(a, _) :: rs               => go(a :: rs, acc) // we decided to take the first root
      case EmptyQuery :: rs                  => go(rs, acc)
    }
    Success(go(List(rule), Nil).reverse)
  }

}
