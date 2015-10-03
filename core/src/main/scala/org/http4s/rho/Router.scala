package org.http4s
package rho

import bits.PathAST._
import bits.HeaderAST._
import bits.QueryAST.QueryRule
import org.http4s.rho.bits.PathTree.ValidationTools
import org.http4s.rho.bits.{ResultResponse, HeaderAppendable, HListToFunc}
import headers.`Content-Type`

import scala.reflect.runtime.universe.{Type, TypeTag}

import shapeless.{::, HList}
import shapeless.ops.hlist.Prepend

import scalaz.concurrent.Task

sealed trait RoutingEntity[T <: HList] {
  def method: Method
  def path: PathRule
  def query: QueryRule
  def headers: HeaderRule
  def validMedia: Set[MediaRange]
  def entityType: Option[Type]
  private[rho] def act(action: Action[T], req: Request, pathstack: HList): ResultResponse[Task[Response]]
}

/** Provides the operations for generating a router
  *
  * @param method request methods to match
  * @param path path matching stack
  * @param headers header validation stack
  * @tparam T cumulative type of the required method for executing the router
  */
case class Router[T <: HList](method: Method,
                              path: PathRule,
                              query: QueryRule,
                              headers: HeaderRule)
                       extends RouteExecutable[T]
                          with HeaderAppendable[T]
                          with RoutingEntity[T]
                          with Decodable[T, Nothing]
{
  override type HeaderAppendResult[T <: HList] = Router[T]

  def validMedia: Set[MediaRange] = Set.empty

  override def entityType: Option[Type] = None

  override def >>>[T1 <: HList](v: TypedHeader[T1])(implicit prep1: Prepend[T1, T]): Router[prep1.Out] =
    Router(method, path, query, HeaderAnd(headers, v.rule))

  override def makeRoute(action: Action[T]): RhoRoute = RhoRoute(this, action)

  override def decoding[R](decoder: EntityDecoder[R])(implicit t: TypeTag[R]): CodecRouter[T, R] =
    CodecRouter(this, decoder)


  override private[rho] def act(action: Action[T], req: Request, pathstack: HList): ResultResponse[Task[Response]] = {
    for {
       i <- ValidationTools.runQuery(req, query, pathstack)
       j <- ValidationTools.runValidation(req, headers, i)
       // `asInstanceOf` to turn the untyped HList to type T
    } yield action.act(req, j.asInstanceOf[action.Tpe])
  }

}

case class CodecRouter[T <: HList, R](router: Router[T], decoder: EntityDecoder[R])(implicit t: TypeTag[R])
           extends HeaderAppendable[T]
           with RouteExecutable[R::T]
           with RoutingEntity[R::T]
           with Decodable[T, R]
{
  override type HeaderAppendResult[T <: HList] = CodecRouter[T, R]

  override def validMedia: Set[MediaRange] = decoder.consumes

  override def >>>[T1 <: HList](v: TypedHeader[T1])(implicit prep1: Prepend[T1, T]): CodecRouter[prep1.Out,R] =
    CodecRouter(router >>> v, decoder)

  /** Append the header to the builder, generating a new typed representation of the route */
//  override def >>>[T2 <: HList](header: TypedHeader[T2])(implicit prep: Prepend[T2, T]): CodecRouter[prep.Out, R] = ???

  override def makeRoute(action: Action[R::T]): RhoRoute = RhoRoute(this, action)

  override def path: PathRule = router.path

  override def method: Method = router.method

  override def query: QueryRule = router.query

  override def decoding[R2 >: R](decoder2: EntityDecoder[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2] =
    CodecRouter(router, decoder orElse decoder2)

  override val headers: HeaderRule = router.headers

  override def entityType: Option[Type] = Some(t.tpe)

  override private[rho] def act(action: Action[R::T], req: Request, pathstack: HList): ResultResponse[Task[Response]] = {
    for {
      i <- ValidationTools.runQuery(req, router.query, pathstack)
      j <- ValidationTools.runValidation(req, headers, i)
    } yield decoder.decode(req).run.flatMap(_.fold(e =>
       Response(Status.BadRequest, req.httpVersion).withBody(e.sanitized),
       { body =>
         // `asInstanceOf` to turn the untyped HList to type T
         action.act(req, (body :: j).asInstanceOf[action.Tpe])
       }))
  }
}

