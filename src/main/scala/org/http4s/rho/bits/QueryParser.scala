package org.http4s.rho.bits

import scalaz.{-\/, \/-, \/}
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

trait QueryParser[A] {
  import QueryParser.Params
  def collect(name: String, params: Params, default: Option[A]): String\/A
}

object QueryParser {
  type Params = Map[String, Seq[String]]

  implicit def optionParse[A](implicit p: StringParser[A]) = new QueryParser[Option[A]] {
    override def collect(name: String, params: Params, default: Option[Option[A]]): \/[String, Option[A]] = {
      params.get(name) match {
        case Some(Seq(v, _*)) => p.parse(v).map(Some(_))
        case Some(Seq())      => -\/(s"Param $name has key but not value")
        case None             => \/-(default.getOrElse(None))
      }
    }
  }

  implicit def multipleParse[A, B[_]](implicit p: StringParser[A], cbf: CanBuildFrom[Seq[_], A, B[A]]) = new QueryParser[B[A]] {
    override def collect(name: String, params: Params, default: Option[B[A]]): \/[String, B[A]] = {
      val b = cbf()
      params.get(name) match {
        case Some(v) =>
          val it = v.iterator
          @tailrec
          def go(): \/[String, B[A]] = {
            if (it.hasNext) {
              p.parse(it.next()) match {
                case \/-(v)    => b += v; go()
                case e@ -\/(_) => e
              }
            } else \/-(b.result)
          }; go()

        case None => \/-(default.getOrElse(b.result))
      }
    }
  }

  implicit def standardCollector[A](implicit p: StringParser[A]) = new QueryParser[A] {
    override def collect(name: String, params: Params, default: Option[A]): \/[String, A] = {
      params.get(name) match {
        case Some(Seq(s, _*)) => p.parse(s)
        case _                => default match {
          case Some(v) => \/-(v)
          case None    => -\/(s"Missing query param: $name")
        }
      }
    }
  }
  
}
