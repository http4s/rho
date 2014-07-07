package org.http4s.rho.bits

import org.http4s.UriTemplate.Path
import org.http4s.UriTemplate.Query

import PathAST.PathRule
import QueryAST.QueryRule

/** Helps to convert different parts of a route into an `UriTemplate` */
object UriConverter {

  def createPath(rule: PathRule): Path = {
    import PathAST._
    import org.http4s.UriTemplate.PathElm
    import org.http4s.UriTemplate.PathExp
    def go(r: PathRule, acc: Path): Path = r match {
      case PathAnd(p1, p2) => go(p1, acc) ++ go(p2, acc)
      case PathOr(p1, p2) => go(p1, acc)
      case PathMatch(s) => PathElm(s) :: acc
      case PathCapture(parser, m) => acc
      case CaptureTail() => acc
      case PathEmpty => acc
      case MetaCons(path, TextMeta(id, desc)) => PathExp(id) :: acc
      case MetaCons(path, meta) => acc
    }
    go(rule, Nil)
  }

  def createQuery(rule: QueryRule): Query = {
    import QueryAST._
    import org.http4s.UriTemplate.ParamExp
    def go(r: QueryRule, acc: Query): Query = r match {
      case MetaCons(query, meta) => acc
      case QueryAnd(a, b) => go(a, acc) ++ go(b, acc)
      case QueryCapture(name, p, default, m) => ParamExp(name) :: acc
      case QueryOr(a, b) => go(a, acc)
      case EmptyQuery => acc
    }
    go(rule, Nil)
  }

}
