package org.http4s
package rho

import org.http4s.rho.bits.{NoMatch, ValidationFailure, ParserFailure, ParserSuccess}
import org.http4s.server.{Service, HttpService}

import org.log4s.getLogger
import shapeless.HList
import scalaz.concurrent.Task

trait RhoService extends bits.MethodAliases
                    with ExecutableCompiler
                    with bits.ResponseGeneratorInstances {

  final protected val logger = getLogger

  private val __tree = new bits.RhoPathTree

  protected def append[T <: HList, F](action: RhoAction[T, F]): Unit =
    __tree.appendAction(action)

  implicit protected def compilerSrvc[F] = new CompileService[F, F] {
    override def compile(action: RhoAction[_ <: HList, F]): F = {
      append(action)
      action.f
    }
  }

  private def findRoute(req: Request): Task[Option[Response]] = {
    logger.info(s"Request: ${req.method}:${req.uri}")
    __tree.getResult(req) match {
      case NoMatch              => Task.now(None)
      case ParserSuccess(t)     => attempt(t).map(Some(_))
      case ParserFailure(s)     => onBadRequest(s).map(Some(_))
      case ValidationFailure(s) => onBadRequest(s).map(Some(_))
    }
  }

  def toService: HttpService = Service.lift(findRoute)

  override def toString(): String = s"RhoService(${__tree.toString()})"

  private def attempt(f: () => Task[Response]): Task[Response] = {
    try f()
    catch { case t: Throwable => onError(t) }
  }


  override def onError(t: Throwable): Task[Response] = {
    logger.error(t)("Error during route execution.")
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
