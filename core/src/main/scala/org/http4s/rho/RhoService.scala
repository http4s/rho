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
                    with LazyLogging
                    with bits.MethodAliases
                    with ExecutableCompiler
                    with bits.ResponseGeneratorInstances {

  private val __tree = new bits.RhoPathTree

  protected def append[T <: HList, F](action: RhoAction[T, F]): Unit =
    __tree.appendAction(action)

  implicit protected def compilerSrvc[F] = new CompileService[F, F] {
    override def compile(action: RhoAction[_ <: HList, F]): F = {
      append(action)
      action.f
    }
  }

  override def isDefinedAt(x: Request): Boolean = __tree.getResult(x) match {
    case NoMatch => false
    case _       => true
  }

  override def apply(req: Request): Task[Response] =
    applyOrElse(req, (_:Request) => throw new MatchError(s"Route not defined at: ${req.uri}"))

  override def applyOrElse[A1 <: Request, B1 >: Task[Response]](x: A1, default: (A1) => B1): B1 = {
    logger.info(s"Request: ${x.method}:${x.uri}")
    __tree.getResult(x) match {
      case NoMatch              => default(x)
      case ParserSuccess(t)     => attempt(t)
      case ParserFailure(s)     => onBadRequest(s)
      case ValidationFailure(s) => onBadRequest(s)
    }
  }

  override def toString(): String = s"RhoService(${__tree.toString()})"

  private def attempt(f: () => Task[Response]): Task[Response] = {
    try f()
    catch { case t: Throwable => onError(t) }
  }


  override def onError(t: Throwable): Task[Response] = {
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
}
