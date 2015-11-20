package org.http4s
package rho.bits

import org.http4s.rho.UriConvertible
import org.http4s.rho.bits.RequestAST._

import shapeless.HList
import shapeless.ops.hlist.Prepend

object QueryAST {

  case class TypedQuery[T <: HList](rule: RequestRule) extends UriConvertible {
    final def or(v: TypedQuery[T]): TypedQuery[T] = TypedQuery(OrRule(this.rule, v.rule))

    final def ||(v: TypedQuery[T]): TypedQuery[T] = or(v)

    final def and[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T1, T]): TypedQuery[prepend.Out] =
      TypedQuery(AndRule(this.rule, v.rule))

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
    import shapeless.{HNil, ::}

    implicit def queryParamKeyLike[T]: QueryParamKeyLike[TypedQuery[T::HNil]] = new QueryParamKeyLike[TypedQuery[T::HNil]] {
      override def getKey(t: TypedQuery[T::HNil]): QueryParameterKey =
        getKey(t.rule).getOrElse(sys.error("Empty Query doesn't have a key name"))

      private def getKey(rule: RequestRule): Option[QueryParameterKey] = rule match {
        case MetaRule(_,QueryMetaData(n,_,_,_)) => Some(QueryParameterKey(n))
        case MetaRule(r,_)                      => getKey(r)
        case OrRule(a, b)                       => getKey(a) orElse getKey(b)
        case IgnoreRule(r)                      => getKey(r)
        case AndRule(a, b)                      => getKey(a) orElse getKey(b) // shouldn't get here
        case CaptureRule(_)                     => None                       // shouldn't get here
        case EmptyRule                          => None                       // shouldn't get here
      }
    }
  }

  private def collectNames(rule: RequestRule): List[String] = {
    @scala.annotation.tailrec
    def go(r: List[RequestRule], acc: List[String]): List[String] = r match {
      case Nil => acc
      case MetaRule(q, QueryMetaData(n,_,_,_)) :: rs  => go(q :: rs, n :: acc)
      case MetaRule(q, _) :: rs                       => go(q :: rs, acc)
      case AndRule(a, b) :: rs                        => go(a :: b :: rs, acc)
      case OrRule(a, _) :: rs                         => go(a :: rs, acc)
      case IgnoreRule(r) :: rs                        => go(r :: rs, acc)
      case (EmptyRule | CaptureRule(_)) :: rs         => go(rs, acc)
    }
    go(List(rule), Nil).reverse
  }
}
