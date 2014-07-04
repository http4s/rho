package org.http4s
package rho

import scala.collection.mutable

import shapeless.{HNil, HList}

import scalaz.concurrent.Task
import scalaz.{-\/, \/-}

trait RhoService extends HttpService with ExecutableCompiler with bits.PathTree {

  private val methods: mutable.Map[Method, Node] = mutable.HashMap.empty

  implicit protected def compilerSrvc[F] = new CompileService[F, F] {
    override def compile(action: RhoAction[_ <: HList, F]): F = {
      append(action)
      action.f
    }
  }

  private def missingMethod = sys.error("Somehow an unknown Method type was found!")

  private def getMethod(method: Method) = methods.get(method).getOrElse(missingMethod)

  protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
    val m = action.method
    val newLeaf = makeLeaf(action)
    val newNode = methods.get(m).getOrElse(HeadNode()).append(action.path, newLeaf)
    methods(m) = newNode
  }

  private def getResult(req: Request): Option[()=>Task[Response]] = {
    val path = req.requestUri.path.split("/").toList match {
      case ""::xs => xs
      case xs     => xs
    }
    getMethod(req.requestMethod).walk(req, path, HNil) match {
      case null => None
      case \/-(t) => Some(t)
      case -\/(s) => Some(() => onBadRequest(s))
    }
  }

  override def isDefinedAt(x: Request): Boolean = getResult(x).isDefined

  override def apply(v1: Request): Task[Response] = getResult(v1)
                                        .map(_.apply())
                                        .getOrElse{throw new MatchError("Route not defined")}

  override def applyOrElse[A1 <: Request, B1 >: Task[Response]](x: A1, default: (A1) => B1): B1 = {
    getResult(x) match {
      case Some(f) => f()
      case None => default(x)
    }
  }

  override def toString(): String = s"RhoService($methods)"
}
