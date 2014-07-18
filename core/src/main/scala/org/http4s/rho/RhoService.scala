package org.http4s
package rho

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.http4s.rho.bits.{ValidationFailure, ParserFailure, ParserSuccess}

import scala.collection.mutable

import shapeless.{HNil, HList}

import scalaz.concurrent.Task

trait RhoService extends server.HttpService with ExecutableCompiler with bits.PathTree with LazyLogging {

  private val methods: mutable.Map[Method, Node] = mutable.HashMap.empty

  protected def onError(t: Throwable): Task[Response] = Status.InternalServerError()

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
    applyOrElse(req, (_:Request) => throw new MatchError(s"Route not defined at: ${req.requestUri}"))

  override def applyOrElse[A1 <: Request, B1 >: Task[Response]](x: A1, default: (A1) => B1): B1 = {
    logger.info(s"Received request: $x")
    getResult(x) match {
      case Some(f) => attempt(f)
      case None => default(x)
    }
  }

  private def getResult(req: Request): Option[()=>Task[Response]] = {
    val path = req.requestUri.path.split("/").toList match {
      case ""::xs => xs
      case     xs => xs
    }
    methods.get(req.requestMethod).flatMap(_.walk(req, path, HNil) match {
      case null => None
      case ParserSuccess(t)     => Some(t)
      case ParserFailure(s)     => Some(() => onBadRequest(s))
      case ValidationFailure(s) => Some(() => onBadRequest(s))
    })
  }

  override def toString(): String = s"RhoService($methods)"

  private def attempt(f: () => Task[Response]): Task[Response] = {
    try f()
    catch { case t: Throwable =>
      logger.error("Error during route execution.", t)
      try onError(t)
      catch {case t: Throwable => Status.InternalServerError(t.getMessage) }
    }
  }
}
