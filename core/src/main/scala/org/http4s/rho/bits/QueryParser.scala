package org.http4s
package rho.bits

import scala.language.higherKinds

import scalaz.{-\/, \/-, \/}
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

trait QueryParser[A] {
  def collect(name: String, req: Request): String\/A
}

object QueryParser {

  implicit def optionParse[A](implicit p: StringParser[A]) = new QueryParser[Option[A]] {
    override def collect(name: String, req: Request): \/[String, Option[A]] = {
      req.params.get(name) match {
        case Some(v) => p.parse(v).map(Some(_))
        case None => \/-(None)
      }
    }
  }

  implicit def multipleParse[A, B[_]](implicit p: StringParser[A], cbf: CanBuildFrom[Seq[_], A, B[A]]) = new QueryParser[B[A]] {
    override def collect(name: String, req: Request): \/[String, B[A]] = {
      val b = cbf()
      req.multiParams.get(name) match {
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

        case None => \/-(b.result)
      }
    }
  }

  implicit def standardCollector[A](implicit p: StringParser[A]) = new QueryParser[A] {
    override def collect(name: String, req: Request): \/[String, A] = {
      req.params.get(name) match {
        case Some(s) => p.parse(s)
        case None    => -\/(s"Missing query param: $name")
      }
    }
  }
  
}
