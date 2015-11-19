package org.http4s
package rho.bits


import scala.language.higherKinds

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom


// TODO: it looks like the default value is _never_ used.
trait QueryParser[A] {
  def collect(name: String, params: Map[String, Seq[String]], default: Option[A]): ResultResponse[A]
}

object QueryParser {
  type Params = Map[String, Seq[String]]

  implicit def optionParse[A](implicit p: StringParser[A]) = new QueryParser[Option[A]] {
    override def collect(name: String, params: Params, default: Option[Option[A]]): ResultResponse[Option[A]] = {
      params.get(name) match {
        case Some(Seq(value, _*)) =>
          p.parse(value) match {
            case SuccessResponse(value) => SuccessResponse(Some(value))
            case other => other.asInstanceOf[ResultResponse[Option[A]]]
          }
        case _ => SuccessResponse(default.getOrElse(None))
      }
    }
  }

  implicit def multipleParse[A, B[_]](implicit p: StringParser[A], cbf: CanBuildFrom[Seq[_], A, B[A]]) = new QueryParser[B[A]] {
    override def collect(name: String, params: Params, default: Option[B[A]]): ResultResponse[B[A]] = {
      val b = cbf()
      params.get(name) match {
        case None => SuccessResponse(default.getOrElse(b.result))
        case Some(Seq()) => SuccessResponse(default.getOrElse(b.result))
        case Some(values) =>
          val it = values.iterator
          @tailrec
          def go(): ResultResponse[B[A]] = {
            if (it.hasNext) {
              p.parse(it.next()) match {
                case SuccessResponse(value) =>
                  b += value
                  go()

                case other => other.asInstanceOf[ResultResponse[B[A]]]
              }
            }
            else SuccessResponse(b.result())
          }; go()
      }
    }
  }

  implicit def standardCollector[A](implicit p: StringParser[A]) = new QueryParser[A] {
    override def collect(name: String, params: Params, default: Option[A]): ResultResponse[A] = {
      params.get(name) match {
        case Some(Seq(value, _*)) => p.parse(value)

        case Some(Seq()) => default match {
          case Some(defaultValue) => SuccessResponse(defaultValue)
          case None => FailureResponse.badRequest(s"Value of query parameter '$name' missing")
        }
        case None => default match {
          case Some(defaultValue) => SuccessResponse(defaultValue)
          case None => FailureResponse.badRequest(s"Missing query param: $name")
        }
      }
    }
  }
}

