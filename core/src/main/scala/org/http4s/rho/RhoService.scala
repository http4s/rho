package org.http4s
package rho

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.http4s.rho.bits.{NoMatch, ValidationFailure, ParserFailure, ParserSuccess}

import scala.annotation.tailrec
import scala.collection.mutable

import shapeless.{HNil, HList}

import scala.collection.mutable.ListBuffer
import scalaz.concurrent.Task

trait RhoService extends server.HttpService
                    with ExecutableCompiler
                    with bits.PathTree
                    with bits.MethodAliases
                    with bits.ResponseGeneratorInstances
                    with LazyLogging {

  private val methods: mutable.Map[Method, Node] = mutable.HashMap.empty

  protected def onError(t: Throwable): Task[Response] = {
    logger.error("Error during route execution.", t)
    val w = Writable.stringWritable
    w.toEntity(t.getMessage).map { entity =>
      val hs = entity.length match {
        case Some(l) => w.headers.put(Header.`Content-Length`(l))
        case None    => w.headers
      }
      Response(Status.InternalServerError, body = entity.body, headers = hs)
    }
  }

  implicit protected def compilerSrvc[F] = new CompileService[F, F] {
    override def compile(action: RhoAction[_ <: HList, F]): F = {
      append(action)
      action.f
    }
  }

  protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
    val m = action.method
    val newLeaf = makeLeaf(action)
    val newNode = methods.get(m).getOrElse(HeadNode()).append(action.path, newLeaf)
    methods(m) = newNode
  }



  override def isDefinedAt(x: Request): Boolean = getResult(x).isDefined

  override def apply(req: Request): Task[Response] =
    applyOrElse(req, (_:Request) => throw new MatchError(s"Route not defined at: ${req.uri}"))

  override def applyOrElse[A1 <: Request, B1 >: Task[Response]](x: A1, default: (A1) => B1): B1 = {
    logger.info(s"Request: ${x.method}:${x.uri}")
    getResult(x) match {
      case Some(f) => attempt(f)
      case None => default(x)
    }
  }

  private def getResult(req: Request): Option[()=>Task[Response]] = {
    val path = splitPath(req.uri.path)
    methods.get(req.method).flatMap(_.walk(req, path, HNil) match {
      case NoMatch              => None
      case ParserSuccess(t)     => Some(t)
      case ParserFailure(s)     => Some(() => onBadRequest(s))
      case ValidationFailure(s) => Some(() => onBadRequest(s))
    })
  }

  private def splitPath(path: String): List[String] = {
    val buff = new ListBuffer[String]
    val len = path.length
    @tailrec
    def go(i: Int, begin: Int): Unit = if (i < len) {
      if (path.charAt(i) == '/') {
        if (i > begin) buff += path.substring(begin, i)
        go(i+1, i+1)
      } else go(i+1, begin)
    } else {
      buff += path.substring(begin, i)
    }

    val i = if (path.charAt(0) == '/') 1 else 0
    go(i,i)

    buff.result
  }

  override def toString(): String = s"RhoService($methods)"

  private def attempt(f: () => Task[Response]): Task[Response] = {
    try f()
    catch { case t: Throwable => onError(t) }
  }
}
