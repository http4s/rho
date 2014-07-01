package org.http4s.rho

import org.http4s.Method
import bits.HListToFunc
import org.http4s.rho.bits.PathAST.{PathAnd, MetaData, PathRule}
import bits.QueryAST._
import org.http4s.rho.bits.HeaderAST.{HeaderRule, EmptyHeaderRule}
import shapeless.ops.hlist.Prepend

import shapeless.{::, HList}


case class QueryBuilder[T <: HList](method: Method,
                        path: PathRule[_ <: HList],
                        query: QueryRule[_ <: HList]) extends RouteExecutable[T] with HeaderAppendable[T]
{
  override type Self = QueryBuilder[T]

  override def makeAction[F, O](f: F, hf: HListToFunc[T, O, F]): RhoAction[T, F, O] =
    RhoAction(Router(method, path, query, validators), f, hf)

  override def >>>[T1 <: HList](v: HeaderRule[T1])(implicit prep1: Prepend[T1, T]): Router[prep1.Out] =
    Router(method, path, query, v)

  override protected def addMetaData(data: MetaData): Self = QueryBuilder(method, PathAnd(path, data), query)

  def &[T1 <: HList](rule: QueryRule[T1])(implicit prep: Prepend[T1, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, path, QueryAnd(query, rule))

  override private[rho] def validators: HeaderRule[_ <: HList] = EmptyHeaderRule
}
