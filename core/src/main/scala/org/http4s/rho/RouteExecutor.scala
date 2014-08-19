package org.http4s
package rho

import bits.PathAST._
import bits.HeaderAST._
import bits.QueryAST._

import org.http4s.Status.BadRequest
import org.http4s.rho.bits._

import shapeless.{HNil, HList, ::}

import scalaz.concurrent.Task
import scalaz.{-\/, \/-, \/}


trait ExecutableCompiler {
  def missingHeader(key: HeaderKey): String = s"Missing header: ${key.name}"

  def missingQuery(key: String): String = s"Missing query param: $key"

  def invalidHeader(h: Header): String = s"Invalid header: $h"

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

  def parsePath(path: String): List[String] = path.split("/").toList

  //////////////////////// Stuff for executing the route //////////////////////////////////////

  /** The untyped guts of ensureValidHeaders and friends */
  protected def runValidation(req: Request, v: HeaderRule, stack: HList): ParserResult[HList] = {
    import bits.HeaderAST.MetaCons
    v match {
      case HeaderAnd(a, b) => runValidation(req, a, stack).flatMap(runValidation(req, b, _))

      case HeaderOr(a, b) => runValidation(req, a, stack).orElse(runValidation(req, b, stack))

      case HeaderCapture(key) => req.headers.get(key) match {
        case Some(h) => ParserSuccess(h::stack)
        case None => ValidationFailure(missingHeader(key))
      }

      case HeaderRequire(key, f) => req.headers.get(key) match {
        case Some(h) => if (f(h)) ParserSuccess(stack) else ValidationFailure(invalidHeader(h))
        case None => ValidationFailure(missingHeader(key))
      }

      case HeaderMapper(key, f) => req.headers.get(key) match {
        case Some(h) => ParserSuccess(f(h)::stack)
        case None => ValidationFailure(missingHeader(key))
      }

      case MetaCons(r, _) => runValidation(req, r, stack)

      case EmptyHeaderRule => ParserSuccess(stack)
    }
  }

  protected def runQuery(req: Request, v: QueryRule, stack: HList): ParserResult[HList] = {
    import QueryAST.MetaCons
    v match {
      case QueryAnd(a, b) => runQuery(req, a, stack).flatMap(runQuery(req, b, _))

      case QueryOr(a, b) => runQuery(req, a, stack).orElse(runQuery(req, b, stack))

      case QueryCapture(name, parser, default, _) => parser.collect(name, req.multiParams, default).map(_ :: stack)

      case MetaCons(r, _) => runQuery(req, r, stack)

      case EmptyQuery => ParserSuccess(stack)
    }
  }

  /** Runs the URL and pushes values to the HList stack */
  protected def runPath(req: Request, v: PathRule, path: List[String]): Option[ParserResult[HList]] = {

    // setup a stack for the path
    var currentPath = path
    def pop = {
      val head = currentPath.head
      currentPath = currentPath.tail
      head
    }

    // WARNING: returns null if not matched but no nulls should escape the runPath method
    def go(v: PathRule, stack: HList): ParserResult[HList] = {
      import PathAST.MetaCons
      v match {
        case PathAnd(a, b) =>
          val v = go(a, stack)
          if (v == null) null
          else if (!currentPath.isEmpty    ||
            b.isInstanceOf[PathAnd]        ||
            b.isInstanceOf[CaptureTail]) v.flatMap(go(b, _))
          else null

        case PathOr(a, b) =>
          val oldPath = currentPath
          val v = go(a, stack)
          if (v != null) v
          else {
            currentPath = oldPath // reset the path stack
            go(b, stack)
          }

        case PathCapture(_, f, _) => f.parse(pop).map{ i => i::stack}

        case PathMatch("") => ParserSuccess(stack)    // "" is consider a NOOP

        case PathMatch(s) =>
          if (pop == s) ParserSuccess(stack)
          else null

        case PathEmpty => // Needs to be the empty path
          if (currentPath.head.length == 0) {
            pop
            ParserSuccess(stack)
          }
          else null

        case CaptureTail() =>
          val p = currentPath
          currentPath = Nil
          ParserSuccess(p::stack)

        case MetaCons(r, _) => go(r, stack)
      }
    }

    if (!path.isEmpty) {
      val r = go(v, HNil)
      if (currentPath.isEmpty) r match {
        case null => None
//        case r@ ParserSuccess(_) => Some(r)
        case r => Some(r)
      } else None
    }
    else None
  }

}

private[rho] class RouteExecutor[F] extends ExecutableCompiler
                                       with CompileService[F, Request=>Option[Task[Response]]] {

  private type Result = Request => Option[Task[Response]]

  ///////////////////// Route execution bits //////////////////////////////////////

  override def compile(action: RhoAction[_ <: HList, F]): Result = action match {
    case RhoAction(r@ Router(_,_,_,_), f, hf)  => compileRouter(r, f, hf)
    case RhoAction(r@ CodecRouter(_,_), f, hf) => compileCodecRouter(r, f, hf)
  }

  protected def compileRouter[T <: HList, F](r: Router[T], f: F, hf: HListToFunc[T, F]): Result = {
    val readyf = hf.conv(f)
    val ff: Result = { req =>
       pathAndValidate(req, r.path, r.query, r.validators).map(_ match {
           case ParserSuccess(stack) => readyf(req, stack.asInstanceOf[T]).map(_.resp)
           case ValidationFailure(s) => onBadRequest(s"Failed validation: $s")
           case ParserFailure(s)     => onBadRequest(s)
       })
    }

    ff
  }
  
  protected def compileCodecRouter[T <: HList, F, R](r: CodecRouter[T, R], f: F, hf: HListToFunc[R::T, F]): Result = {
    val actionf = hf.conv(f)        // Cache our converted function
    val ff: Result = { req =>
      // TODO: how to handle decoder error, using the Task error handling, special exceptions, or disjunctions?
      pathAndValidate(req, r.router.path, r.router.query, r.validators).map(_ match {
        case ParserSuccess(stack) =>
          r.decoder.decode(req)                                  // Decode the body of the `Request`
            .flatMap( r => actionf(req,r::stack.asInstanceOf[T]) // append body to path and query and feed to the actionf
            .map(_.resp))                                        // extract the `Response` from the `Result[T]`

        case ValidationFailure(s) => onBadRequest(s"Failed validation: $s")
        case ParserFailure(s)     => onBadRequest(s)
      })
    }

    ff
  }

  private def pathAndValidate(req: Request,
                             path: PathRule,
                            query: QueryRule,
                                v: HeaderRule): Option[ParserResult[HList]] =
  {
    val p = parsePath(req.uri.path)

    runPath(req, path, p).map { r =>
      for {
        i <- r
        j <- runQuery(req, query, i)
        k <- runValidation(req, v, j)
      } yield k
    }
  }

  /** Walks the validation tree */
  def ensureValidHeaders(v: HeaderRule, req: Request): ParserResult[HList] =
    runValidation(req, v, HNil)
}
