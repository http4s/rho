package org.http4s
package rho.bits

import scala.language.existentials

import org.http4s.rho.UriConvertible

import shapeless.{HNil, HList, :: => :#:}
import shapeless.ops.hlist.Prepend

import scalaz.Applicative

object QueryAST {

  /** A TypedQuery acts as a shell to maintain type safety of the [[QueryRule]] construction and evaluation. */
  case class TypedQuery[T <: HList](rule: QueryRule) extends UriConvertible {

    final def or(v: TypedQuery[T]): TypedQuery[T] = TypedQuery(QueryOr(this.rule, v.rule))

    final def ||(v: TypedQuery[T]): TypedQuery[T] = or(v)

    final def and[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T1, T]): TypedQuery[prepend.Out] =
      TypedQuery(QueryAnd(this.rule, v.rule))

    final def &&[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T1, T]): TypedQuery[prepend.Out] = and(v)

    final def &[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T1, T]): TypedQuery[prepend.Out] = and(v)

    /**
     * Resolves names of query parameters to capture
     */
    val names: List[String] = collectNames(rule)

    private val uriTemplate =
      for (q <- UriConverter.createQuery(rule))
        yield UriTemplate(query = q)

    override def asUriTemplate(request: Request) =
      UriConvertible.respectPathInfo(uriTemplate, request)
  }

  object TypedQuery {
//    import shapeless.{HNil, ::}
//    implicit def queryParamKeyLike[T]: QueryParamKeyLike[TypedQuery[T::HNil]] = new QueryParamKeyLike[TypedQuery[T::HNil]] {
//      override def getKey(t: TypedQuery[T::HNil]): QueryParameterKey =
//        getKey(t.rule).getOrElse(sys.error("Empty Query doesn't have a key name"))
//
//      private def getKey(rule: QueryRule): Option[QueryParameterKey] = rule match {
//        case QueryCapture(n,_,_,_) => Some(QueryParameterKey(n))
//        case QueryOr(a, b)         => getKey(a) orElse getKey(b)
//        case MetaCons(r,_)         => getKey(r)
//        case QueryAnd(a, b)        => getKey(a) orElse getKey(b) // shouldn't get here
//        case EmptyQuery            => None                       // shouldn't get here
//      }
//    }

    def typeQueryApplicativeInstance[T]: Applicative[({type t[A] = TypedQuery[A:#:HNil]})#t] = new Applicative[({type t[A] = TypedQuery[A:#:HNil]})#t] {
      override def point[A](a: => A): TypedQuery[A:#:HNil] = TypedQuery(QueryCapture(RequestReader.point(a)))

      override def ap[A, B](fa: => TypedQuery[A:#:HNil])(f: => TypedQuery[(A => B):#:HNil]): TypedQuery[B:#:HNil] = {
        ??? // TODO: this is will be nasty due to 'or' rules...
      }
    }
  }

  sealed trait QueryRule

  case class QueryCapture(reader: RequestReader[_]) extends QueryRule

  case class QueryAnd(a: QueryRule, b: QueryRule) extends QueryRule

  case class QueryOr(a: QueryRule, b: QueryRule) extends QueryRule

  case class MetaCons(query: QueryRule, meta: Metadata) extends QueryRule

  case object EmptyQuery extends QueryRule

  private def collectNames(rule: QueryRule): List[String] = {

    def fastPrepend[A](a: Seq[A], b: List[A]): List[A] = {
      import scala.collection.mutable.ListBuffer
      val buff = new ListBuffer[A]
      buff ++= a
      buff.prependToList(b)
    }

    @scala.annotation.tailrec
    def go(r: List[QueryRule], acc: List[String]): List[String] = r match {
      case Nil => acc
      case MetaCons(query, _) :: rs => go(rs, acc)
      case QueryAnd(a, b) :: rs => go(a :: b :: rs, acc)
      case QueryCapture(reader) :: rs => go(rs, fastPrepend(RequestReader.queryNames(reader),acc))
      case QueryOr(a, _) :: rs => go(a :: rs, acc)
      case EmptyQuery :: rs => go(rs, acc)
    }
    go(List(rule), Nil).reverse
  }

}
