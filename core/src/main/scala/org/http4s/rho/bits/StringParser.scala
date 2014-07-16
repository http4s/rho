package org.http4s
package rho.bits

import scalaz.{-\/, \/-, \/}

import scala.reflect.runtime.universe.TypeTag

trait StringParser[T] {
  def parse(s: String): ParserResult[T]
  def typeTag: Option[TypeTag[T]]
}

object StringParser {

  ////////////////////// Default parsers //////////////////////////////

  implicit val intParser = new StringParser[Int] {
    override val typeTag: Some[TypeTag[Int]] = Some(implicitly[TypeTag[Int]])
    override def parse(s: String): ParserResult[Int] =
      try ParserSuccess(s.toInt)
      catch { case e: NumberFormatException => ParserFailure(s"Invalid Number Format: $s") }
  }

  implicit val strParser = new StringParser[String] {
    override val typeTag: Some[TypeTag[String]] = Some(implicitly[TypeTag[String]])
    override def parse(s: String): ParserResult[String] = ParserSuccess(s)
  }

//  implicit def optionParser[A](implicit p: StringParser[A], m: TypeTag[Option[A]]) = new StringParser[Option[A]] {
//    override def typeTag: Some[TypeTag[Option[A]]] = Some(m)
//    override def parse(s: String): ParserResult[Option[A]] = p.parse(s) match {
//      case \/-(r) => ParserSuccess(Some(r))
//      case -\/(_) => \/-(None)
//    }
//  }

}
