package org.http4s
package rho

import bits.PathAST._
import bits.HeaderAST.HeaderRule

import org.http4s.rho.bits.HListToFunc

import scala.collection.mutable

import shapeless.{HNil, HList}

import scalaz.concurrent.Task
import scalaz.{-\/, \/-, \/}

/**
* Created by Bryce Anderson on 4/30/14.
*/
trait RhoService extends HttpService with ExecutableCompiler with bits.PathTree {

  private val methods: mutable.Map[Method, Node] = mutable.HashMap.empty

  implicit protected def compilerSrvc = new CompileService[Unit] {
    override def compile[T <: HList, F, O](action: RhoAction[T, F, O]): Unit = append(action)
  }

  private def missingMethod = sys.error("Somehow an unknown Method type was found!")

  private def getMethod(method: Method) = methods.get(method).getOrElse(missingMethod)

  protected def append[T <: HList, F, O](action: RhoAction[T, F, O]): Unit = {
    val m = action.router.method
    val newLeaf = makeLeaf(action)
    val newNode = methods.get(m).getOrElse(HeadNode()).append(action.router.path, newLeaf)
    methods(m) = newNode
  }

  private def getResult(req: Request): Option[()=>Task[Response]] = {
    val path = req.requestUri.path.split("/").toList
    getMethod(req.requestMethod).walk(req, path, HNil) match {
      case null => None
      case \/-(t) => Some(t)
      case -\/(s) => Some(()=>onBadRequest(s))
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

  override def toString(): String = s"CoolService($methods)"
}

case class RhoAction[T <: HList, F, O](private[rho] val router: RouteExecutable[T],
                                       private[rho] val f: F,
                                       private[rho] val hf: HListToFunc[T, O, F]) {
  final def method: Method = router.method
  final def path: PathRule[_ <: HList] = router.path
  final def validators: HeaderRule[_ <: HList] = router.validators
  final def responseEncodings: Seq[MediaType] = hf.encodings
  final def responseType: Option[Manifest[O]] = hf.manifest
  final def decoders: Seq[MediaType] = router match {
    case r: CodecRouter[_,_] => r.decoder.consumes
    case _ => Nil
  }
}
