package org.http4s
package rho

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
  * @tparam T cumulative type of the required method for executing the router
  */
case class Router[T <: HList](method: Method,
                               val path: PathRule,
                               val query: QueryRule,
                               validators: HeaderRule)
                       extends RouteExecutable[T]
                          with HeaderAppendable[T]
                          with MetaDataSyntax
{

  type Self = Router[T]

  override def >>>[T2 <: HList](v: TypedHeader[T2])(implicit prep1: Prepend[T2, T]): Router[prep1.Out] =
    Router(method, path, query, HeaderAnd(validators, v.rule))

  override def makeAction[F](f: F, hf: HListToFunc[T, F]): RhoAction[T, F] =
    new RhoAction(this, f, hf)

  def decoding[R](decoder: Decoder[R]): CodecRouter[T, R] = CodecRouter(this, decoder)

  override def addMetaData(data: Metadata): Router[T] =
    Router(method, path, query, MetaCons(validators, data))
}

case class CodecRouter[T <: HList, R](router: Router[T], decoder: Decoder[R])
           extends HeaderAppendable[T]
           with RouteExecutable[R::T]
           with MetaDataSyntax
{
  type Self = CodecRouter[T, R]

  override def addMetaData(data: Metadata): CodecRouter[T, R] =
    CodecRouter(router.addMetaData(data), decoder)

  override def >>>[T2 <: HList](v: TypedHeader[T2])(implicit prep1: Prepend[T2, T]): CodecRouter[prep1.Out,R] =
    CodecRouter(router >>> v, decoder)

  override def makeAction[F](f: F, hf: HListToFunc[R::T, F]): RhoAction[R::T, F] =
    new RhoAction(this, f, hf)

  override def path: PathRule = router.path

  override def method: Method = router.method

  override def query: QueryRule = router.query

  def decoding(decoder2: Decoder[R]): CodecRouter[T, R] = CodecRouter(router, decoder.or(decoder2))

  override val validators: HeaderRule = {
    if (!decoder.consumes.isEmpty) {
      val mt = requireThat(Header.`Content-Type`){ h: Header.`Content-Type`.HeaderT =>
        decoder.consumes.find(_ == h.mediaType).isDefined
      }
      HeaderAnd(router.validators, mt.rule)
    } else router.validators
  }
}

private[rho] trait RouteExecutable[T <: HList] {
  def method: Method

  def path: PathRule

  def query: QueryRule

  def validators: HeaderRule

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
