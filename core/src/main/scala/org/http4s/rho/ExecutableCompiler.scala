package org.http4s
package rho

import bits.HeaderAST._
import bits.QueryAST._
import org.http4s.rho.bits.ResponseGeneratorInstances.BadRequest

import org.http4s.rho.bits._

import shapeless.{ HNil, HList, :: }


trait ExecutableCompiler {

  def parsePath(path: String): List[String] = path.split("/").toList

  //////////////////////// Stuff for executing the route //////////////////////////////////////

  /** Walks the validation tree */
  def ensureValidHeaders(v: HeaderRule, req: Request): ParserResult[HList] =
    runValidation(req, v, HNil)

  /** The untyped guts of ensureValidHeaders and friends */
  def runValidation(req: Request, v: HeaderRule, stack: HList): ParserResult[HList] = {
    import bits.HeaderAST.MetaCons
    v match {
      case HeaderAnd(a, b) => runValidation(req, a, stack).flatMap(runValidation(req, b, _))

      case HeaderOr(a, b) => runValidation(req, a, stack).orElse(runValidation(req, b, stack))

      case HeaderExists(key, f) => req.headers.get(key) match {
        case Some(h) => f(h).fold[ParserResult[HList]](ParserSuccess(stack))(ValidationFailure.result)
        case None => ValidationFailure.result(BadRequest(s"Missing header: ${key.name}"))
      }

      case HeaderCapture(key, f, default) => req.headers.get(key) match {
        case Some(h) =>
          f(h).fold(f => ValidationFailure.result(f), r => ParserSuccess(r::stack))

        case None => default match {
          case Some(r) => ValidationFailure.result(r)
          case None    => ValidationFailure.result(BadRequest(s"Missing header: ${key.name}"))
        }
      }

      case MetaCons(r, _) => runValidation(req, r, stack)

      case EmptyHeaderRule => ParserSuccess(stack)
    }
  }

  def runQuery(req: Request, v: QueryRule, stack: HList): ParserResult[HList] = {
    import QueryAST.MetaCons
    v match {
      case QueryAnd(a, b) => runQuery(req, a, stack).flatMap(runQuery(req, b, _))

      case QueryOr(a, b) => runQuery(req, a, stack).orElse(runQuery(req, b, stack))

      case QueryCapture(name, parser, default, _) => parser.collect(name, req.multiParams, default).map(_ :: stack)

      case MetaCons(r, _) => runQuery(req, r, stack)

      case EmptyQuery => ParserSuccess(stack)
    }
  }
}
