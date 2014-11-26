package org.http4s
package rho

import bits.PathAST._
import bits.HeaderAST._
import bits.QueryAST._

import org.http4s.Status.{BadRequest, InternalServerError}
import org.http4s.rho.bits._

import shapeless.{HNil, HList, ::}

import scalaz.concurrent.Task


trait ExecutableCompiler {
  def onBadRequest(reason: String): Task[Response] = {
    val w = Writable.stringWritable
    w.toEntity(reason).map{ entity =>
      val hs = entity.length match {
        case Some(l) => w.headers.put(Header.`Content-Length`(l))
        case None    => w.headers
      }
      Response(BadRequest, body = entity.body, headers = hs)
    }
  }

  def onError(t: Throwable): Task[Response] =
      Task.now(Response(InternalServerError))

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

      case HeaderCapture(key) => req.headers.get(key) match {
        case Some(h) => ParserSuccess(h::stack)
        case None => ValidationFailure(s"Missing header: ${key.name}")
      }

      case HeaderRequire(key, f) => req.headers.get(key) match {
        case Some(h) => if (f(h)) ParserSuccess(stack) else ValidationFailure(s"Invalid header: $h")
        case None => ValidationFailure(s"Missing header: ${key.name}")
      }

      case HeaderMapper(key, f) => req.headers.get(key) match {
        case Some(h) => ParserSuccess(f(h)::stack)
        case None => ValidationFailure(s"Missing header: ${key.name}")
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
