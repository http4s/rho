package org.http4s
package rho.bits

import cats.Monad
import org.http4s.rho.bits.QueryParser.Params

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.generic.CanBuildFrom

/** Extract a value from the `Request` `Query`
  *
  * @tparam A Type of value produced by the parser.
  */
trait QueryParser[F[_], A] {

  def collect(name: String, params: Params, default: Option[A]): ResultResponse[F, A]
}

object QueryParser {
  type Params = Map[String, Seq[String]]
}

trait QueryParsers[F[_]] extends FailureResponseOps[F] {

  /** Optionally extract the value from the `Query` */
  implicit def optionParse[A](implicit F: Monad[F], p: StringParser[F, A]) = new QueryParser[F, Option[A]] {
    override def collect(name: String, params: Params, default: Option[Option[A]]): ResultResponse[F, Option[A]] = {
      params.get(name) match {
        case Some(Seq(value, _*)) =>
          p.parse(value) match {
            case SuccessResponse(successValue) => SuccessResponse(Some(successValue))
            case other => other.asInstanceOf[ResultResponse[F, Option[A]]]
          }
        case _ => SuccessResponse(default.flatten)
      }
    }
  }

  /** Extract multiple elements from the `Query`
    *
    * The elements must have the same name and each be a valid representation of the requisite type.
    */
  implicit def multipleParse[A, B[_]](implicit F: Monad[F], p: StringParser[F, A], cbf: CanBuildFrom[Seq[_], A, B[A]]) = new QueryParser[F, B[A]] {
    override def collect(name: String, params: Params, default: Option[B[A]]): ResultResponse[F, B[A]] = {
      val b = cbf(Seq.empty)
      params.get(name) match {
        case None => SuccessResponse(default.getOrElse(b.result))
        case Some(Seq()) => SuccessResponse(default.getOrElse(b.result))
        case Some(values) =>
          val it = values.iterator
          @tailrec
          def go(): ResultResponse[F, B[A]] = {
            if (it.hasNext) {
              p.parse(it.next()) match {
                case SuccessResponse(value) =>
                  b += value
                  go()

                case other => other.asInstanceOf[ResultResponse[F, B[A]]]
              }
            }
            else SuccessResponse(b.result())
          }; go()
      }
    }
  }

  /** Extract an element from the `Query` using a [[StringParser]] */
  implicit def standardCollector[A](implicit F: Monad[F], p: StringParser[F, A]) = new QueryParser[F, A] {
    override def collect(name: String, params: Params, default: Option[A]): ResultResponse[F, A] = {
      params.get(name) match {
        case Some(Seq(value, _*)) => p.parse(value)

        case Some(Seq()) => default match {
          case Some(defaultValue) => SuccessResponse(defaultValue)
          case None => badRequest(s"Value of query parameter '$name' missing")
        }
        case None => default match {
          case Some(defaultValue) => SuccessResponse(defaultValue)
          case None => badRequest(s"Missing query param: $name")
        }
      }
    }
  }
}
