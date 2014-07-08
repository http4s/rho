package org.http4s
package rho.bits

import scalaz.{-\/, \/-, \/}

import scala.reflect.runtime.universe.TypeTag

trait StringParser[T] {
  def parse(s: String): \/[String, T]
  def typeTag: Option[TypeTag[T]]
}

object StringParser {

  ////////////////////// Default parsers //////////////////////////////

  implicit val intParser = new StringParser[Int] {
    override val typeTag: Some[TypeTag[Int]] = Some(implicitly[TypeTag[Int]])
    override def parse(s: String): \/[String,Int] =
      try \/-(s.toInt)
      catch { case e: NumberFormatException => -\/(s"Invalid Number Format: $s") }
  }

  implicit val strParser = new StringParser[String] {
    override val typeTag: Some[TypeTag[String]] = Some(implicitly[TypeTag[String]])
    override def parse(s: String): \/[String,String] = \/-(s)
  }

  implicit def optionParser[A](implicit p: StringParser[A], m: TypeTag[Option[A]]) = new StringParser[Option[A]] {
    override def typeTag: Some[TypeTag[Option[A]]] = Some(m)
    override def parse(s: String): \/[String, Option[A]] = p.parse(s) match {
      case \/-(r) => \/-(Some(r))
      case -\/(_) => \/-(None)
    }
  }

}
