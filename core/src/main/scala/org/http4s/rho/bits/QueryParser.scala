package org.http4s
package rho.bits

import org.http4s.rho.Result.BaseResult
import org.http4s.rho.bits.QueryParser.Params

import scala.language.higherKinds

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scalaz.concurrent.Task

trait QueryParser[A] {
  import QueryParser.Params
  def collect(name: String, params: Params, default: Option[A]): ParserResult[A]
}

final class ValidatingParser[A](parent: QueryParser[A], validate: A => Option[Task[BaseResult]]) extends QueryParser[A] {
  override def collect(name: String, params: Params, default: Option[A]): ParserResult[A] = {
    val result = parent.collect(name, params, default)
    result.flatMap{ r => validate(r) match {
        case None       => result
        case Some(resp) => ParserFailure.pure(resp.map(_.resp))
      }
    }
  }
}

object QueryParser {
  type Params = Map[String, Seq[String]]

  implicit def optionParse[A](implicit p: StringParser[A]) = new QueryParser[Option[A]] {
    override def collect(name: String, params: Params, default: Option[Option[A]]): ParserResult[Option[A]] = {
      val defaultValue = default.getOrElse(None)
      params.get(name) match {
        case Some(Seq(value, _*)) =>
          p.parse(value) match {
            case ParserSuccess(value) => ParserSuccess(Some(value))
            case other => other.asInstanceOf[ParserResult[Option[A]]]
          }
        case _ => ParserSuccess(default.getOrElse(None))
      }
    }
  }

  implicit def multipleParse[A, B[_]](implicit p: StringParser[A], cbf: CanBuildFrom[Seq[_], A, B[A]]) = new QueryParser[B[A]] {
    override def collect(name: String, params: Params, default: Option[B[A]]): ParserResult[B[A]] = {
      val b = cbf()
      params.get(name) match {
        case None => ParserSuccess(default.getOrElse(b.result))
        case Some(Seq()) => ParserSuccess(default.getOrElse(b.result))
        case Some(values) =>
          val it = values.iterator
          @tailrec
          def go(): ParserResult[B[A]] = {
            if (it.hasNext) {
              p.parse(it.next()) match {
                case ParserSuccess(value) =>
                  b += value
                  go()

                case other => other.asInstanceOf[ParserResult[B[A]]]
              }
            }
            else ParserSuccess(b.result())
          }; go()
      }
    }
  }

  implicit def standardCollector[A](implicit p: StringParser[A]) = new QueryParser[A] {
    override def collect(name: String, params: Params, default: Option[A]): ParserResult[A] = {
      params.get(name) match {
        case Some(Seq(value, _*)) => p.parse(value)

        case Some(Seq()) => default match {
          case Some(defaultValue) => ParserSuccess(defaultValue)
          case None => ParserFailure.badRequest(s"Value of query parameter '$name' missing")
        }
        case None => default match {
          case Some(defaultValue) => ParserSuccess(defaultValue)
          case None => ParserFailure.badRequest(s"Missing query param: $name")
        }
      }
    }
  }
}

