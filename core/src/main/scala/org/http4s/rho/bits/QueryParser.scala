package org.http4s
package rho.bits

import scala.language.higherKinds

import scalaz.{-\/, \/-, \/}
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

trait QueryParser[A] {
  import QueryParser.Params
  def collect(name: String, params: Params, default: Option[A], accept: A => Boolean): String\/A
}

object QueryParser {
  type Params = Map[String, Seq[String]]

  implicit def optionParse[A](implicit p: StringParser[A]) = new QueryParser[Option[A]] {
    override def collect(name: String, params: Params, default: Option[Option[A]], validate: Option[A] => Boolean): \/[String, Option[A]] = {
      val defaultValue = default.getOrElse(None)
      params.get(name) match {
        case Some(Seq(value, _*)) =>
          p.parse(value) match {
            case unparsable @ -\/(_) => default match {
              case Some(defaultValue) => \/-(defaultValue)
              case None => \/-(None)
            }
            case parsed @ \/-(value) =>
              if (validate(Some(value))) \/-(Some(value))
              else default match {
                case Some(defaultValue) => \/-(defaultValue)
                case None => \/-(None)
              }
          }
        case Some(Seq()) => \/-(default.getOrElse(None))
        case None => \/-(default.getOrElse(None))
      }
    }
  }

  implicit def multipleParse[A, B[_]](implicit p: StringParser[A], cbf: CanBuildFrom[Seq[_], A, B[A]]) = new QueryParser[B[A]] {
    override def collect(name: String, params: Params, default: Option[B[A]], validate: B[A] => Boolean): \/[String, B[A]] = {
      val b = cbf()
      params.get(name) match {
        case None => \/-(default.getOrElse(b.result))
        case Some(Seq()) => \/-(default.getOrElse(b.result))
        case Some(values) =>
          val it = values.iterator
          @tailrec
          def go(): \/[String, B[A]] = {
            if (it.hasNext) {
              p.parse(it.next()) match {
                case unparsable @ -\/(_) => unparsable
                case parsed @ \/-(value) =>
                  b += value
                  go()
              }
            } else default match {
              case None => \/-(b.result)
              case Some(defaultValues) =>
                val parsedValues = b.result
                if (validate(parsedValues)) \/-(parsedValues)
                else \/-(defaultValues)
            }
          }; go()
      }
    }
  }

  implicit def standardCollector[A](implicit p: StringParser[A]) = new QueryParser[A] {
    override def collect(name: String, params: Params, default: Option[A], validate: A => Boolean): \/[String, A] = {
      params.get(name) match {
        case Some(Seq(value, _*)) =>
          p.parse(value) match {
            case unparsable @ -\/(_) => unparsable
            case parsed @ \/-(value) =>
              default match {
                case None => parsed
                case Some(defaultValue) =>
                  if (validate(value)) parsed
                  else \/-(defaultValue)
              }
          }
        case Some(Seq()) => default match {
          case Some(defaultValue) => \/-(defaultValue)
          case None => -\/(s"Value of query parameter '$name' missing")
        }
        case None => default match {
          case Some(defaultValue) => \/-(defaultValue)
          case None => -\/(s"Missing query param: $name")
        }
      }
    }
  }

}
