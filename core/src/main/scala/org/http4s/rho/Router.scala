package org.http4s
package rho

import scala.language.existentials

import bits.PathAST._
import bits.HeaderAST._
import bits.QueryAST.QueryRule
import bits.HeaderAST.MetaCons
import org.http4s.rho.bits.{HeaderAppendable, Metadata, MetaDataSyntax, HListToFunc}

import shapeless.{::, HList}
import scalaz.concurrent.Task
import shapeless.ops.hlist.Prepend

/** Provides the operations for generating a router
  *
  * @param method request methods to match
  * @param path path matching stack
  * @param validators header validation stack
  * @tparam T1 cumulative type of the required method for executing the router
  */
case class Router[T1 <: HList](method: Method,
                               private[rho] val path: PathRule[_ <: HList],
                               private[rho] val query: QueryRule[_ <: HList],
                               validators: HeaderRule[_ <: HList])
                       extends RouteExecutable[T1]
                          with HeaderAppendable[T1]
                          with MetaDataSyntax
{

  type Self = Router[T1]

  override def >>>[T3 <: HList](v: HeaderRule[T3])(implicit prep1: Prepend[T3, T1]): Router[prep1.Out] =
    Router(method, path, query, HeaderAnd(validators,v))

  override def makeAction[F](f: F, hf: HListToFunc[T1, F]): RhoAction[T1, F] =
    new RhoAction(this, f, hf)

  def decoding[R](decoder: Decoder[R]): CodecRouter[T1, R] = CodecRouter(this, decoder)

  override def addMetaData(data: Metadata): Router[T1] =
    Router(method, path, query, MetaCons(validators, data))
}

case class CodecRouter[T1 <: HList, R](router: Router[T1], decoder: Decoder[R])
           extends HeaderAppendable[T1]
           with RouteExecutable[R::T1]
           with MetaDataSyntax
{
  type Self = CodecRouter[T1, R]

  override def addMetaData(data: Metadata): CodecRouter[T1, R] =
    CodecRouter(router.addMetaData(data), decoder)

  override def >>>[T3 <: HList](v: HeaderRule[T3])(implicit prep1: Prepend[T3, T1]): CodecRouter[prep1.Out,R] =
    CodecRouter(router >>> v, decoder)

  override def makeAction[F](f: F, hf: HListToFunc[R::T1, F]): RhoAction[R::T1, F] =
    new RhoAction(this, f, hf)

  private[rho] override def path: PathRule[_ <: HList] = router.path

  override def method: Method = router.method

  def decoding(decoder2: Decoder[R]): CodecRouter[T1, R] = CodecRouter(router, decoder.or(decoder2))

  override private[rho] val validators: HeaderRule[_ <: HList] = {
    if (!decoder.consumes.isEmpty) {
      val mt = requireThat(Header.`Content-Type`){ h: Header.`Content-Type`.HeaderT =>
        decoder.consumes.find(_ == h.mediaType).isDefined
      }
      HeaderAnd(router.validators, mt)
    } else router.validators
  }
}

private[rho] trait RouteExecutable[T <: HList] {
  def method: Method

  private[rho] def path: PathRule[_ <: HList]
//
  private[rho] def validators: HeaderRule[_ <: HList]

  def makeAction[F](f: F, hf: HListToFunc[T, F]): RhoAction[T, F]

  /** Compiles a HTTP request definition into an action */
  final def |>>[F, R](f: F)(implicit hf: HListToFunc[T, F], srvc: CompileService[F, R]): R =
    compile(f)(hf, srvc)

  /** Compiles a HTTP request definition into an action */
  final def compile[F, R](f: F)(implicit hf: HListToFunc[T, F], srvc: CompileService[F, R]): R =
    srvc.compile(makeAction(f, hf))

  final def runWith[F, O, R](f: F)(implicit hf: HListToFunc[T, F]): Request => Option[Task[Response]] =
    compile(f)(hf, new RouteExecutor)
}
