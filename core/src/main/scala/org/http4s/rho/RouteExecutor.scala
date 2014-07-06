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

import Decoder._



trait ExecutableCompiler {
  def missingHeader(key: HeaderKey): String = s"Missing header: ${key.name}"

  def missingQuery(key: String): String = s"Missing query param: $key"

  def invalidHeader(h: Header): String = s"Invalid header: $h"

  def onBadRequest(reason: String): Task[Response] = BadRequest(reason)

  def parsePath(path: String): List[String] = path.split("/").toList

  //////////////////////// Stuff for executing the route //////////////////////////////////////

  /** The untyped guts of ensureValidHeaders and friends */
  protected def runValidation(req: Request, v: HeaderRule, stack: HList): \/[String,HList] = {
    import bits.HeaderAST.MetaCons
    v match {
      case HeaderAnd(a, b) => runValidation(req, a, stack).flatMap(runValidation(req, b, _))

      case HeaderOr(a, b) => runValidation(req, a, stack).orElse(runValidation(req, b, stack))

      case HeaderCapture(key) => req.headers.get(key) match {
        case Some(h) => \/-(h::stack)
        case None => -\/(missingHeader(key))
      }

      case HeaderRequire(key, f) => req.headers.get(key) match {
        case Some(h) => if (f(h)) \/-(stack) else -\/(invalidHeader(h))
        case None => -\/(missingHeader(key))
      }

      case HeaderMapper(key, f) => req.headers.get(key) match {
        case Some(h) => \/-(f(h)::stack)
        case None => -\/(missingHeader(key))
      }

      case MetaCons(r, _) => runValidation(req, r, stack)

      case EmptyHeaderRule => \/-(stack)
    }
  }

  protected def runQuery(req: Request, v: QueryRule, stack: HList): String\/HList = {
    import QueryAST.MetaCons
    v match {
      case QueryAnd(a, b) => runQuery(req, a, stack).flatMap(runQuery(req, b, _))

      case QueryOr(a, b) => runQuery(req, a, stack).orElse(runQuery(req, b, stack))

      case QueryCapture(name, parser, default, _) => parser.collect(name, req.multiParams, default).map(_ :: stack)

      case MetaCons(r, _) => runQuery(req, r, stack)

      case EmptyQuery => \/-(stack)
    }
  }

  /** Runs the URL and pushes values to the HList stack */
  protected def runPath(req: Request, v: PathRule, path: List[String]): Option[\/[String, HList]] = {

    // setup a stack for the path
    var currentPath = path
    def pop = {
      val head = currentPath.head
      currentPath = currentPath.tail
      head
    }

    // WARNING: returns null if not matched but no nulls should escape the runPath method
    def go(v: PathRule, stack: HList): \/[String,HList] = {
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

        case PathCapture(f, _) => f.parse(pop).map{ i => i::stack}

        case PathMatch("") => \/-(stack)    // "" is consider a NOOP

        case PathMatch(s) =>
          if (pop == s) \/-(stack)
          else null

        case PathEmpty => // Needs to be the empty path
          if (currentPath.head.length == 0) {
            pop
            \/-(stack)
          }
          else null

        case CaptureTail() =>
          val p = currentPath
          currentPath = Nil
          \/-(p::stack)

        case MetaCons(r, _) => go(r, stack)
      }
    }

    if (!path.isEmpty) {
      val r = go(v, HNil)
      if (currentPath.isEmpty) r match {
        case null => None
        case r@ \/-(_) => Some(r.asInstanceOf[\/[String,HList]])
        case r@ -\/(_) => Some(r)
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
           case \/-(stack) => readyf(req, stack.asInstanceOf[T])
           case -\/(s) => onBadRequest(s)
       })
    }

    ff
  }
  
  protected def compileCodecRouter[T <: HList, F, R](r: CodecRouter[T, R], f: F, hf: HListToFunc[R::T, F]): Result = {
    val actionf = hf.conv(f)
    val allvals = {
      if (!r.decoder.force) {
        val mediaReq: Seq[HeaderRule] = r.decoder.consumes.map { mediaType =>
          HeaderRequire(Header.`Content-Type`, { h: Header.`Content-Type`.HeaderT => h.mediaType == mediaType })
        }
        if (mediaReq.isEmpty) r.router.validators
        else HeaderAnd(r.router.validators, mediaReq.tail.foldLeft(mediaReq.head)(HeaderOr(_, _)))
      }
      else r.router.validators
    }
    val ff: Result = { req =>
      pathAndValidate(req, r.router.path, r.router.query, allvals).map(_ match {
        case \/-(stack) => r.decoder.decode(req).flatMap(_ match {
            case \/-(r) => actionf(req,r::stack.asInstanceOf[T])
            case -\/(e) => onBadRequest(s"Error decoding body: $e")
          })
        case -\/(s) => onBadRequest(s)
      })
    }

    ff
  }

  private def pathAndValidate(req: Request,
                             path: PathRule,
                            query: QueryRule,
                                v: HeaderRule): Option[\/[String, HList]] =
  {
    val p = parsePath(req.requestUri.path)

    runPath(req, path, p).map { r =>
      for {
        i <- r
        j <- runQuery(req, query, i)
        k <- runValidation(req, v, j)
      } yield k
    }.asInstanceOf[Option[\/[String, HList]]]
  }

  /** Walks the validation tree */
  def ensureValidHeaders(v: HeaderRule, req: Request): \/[String, HList] =
    runValidation(req, v, HNil).asInstanceOf[\/[String,HList]]
}
