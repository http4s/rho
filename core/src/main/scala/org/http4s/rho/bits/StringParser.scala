package org.http4s
package rho.bits

import scala.reflect.runtime.universe.TypeTag

trait StringParser[T] {
  def parse(s: String): ParserResult[T]
  def typeTag: Option[TypeTag[T]]
}

class BooleanParser extends StringParser[Boolean] {
  override val typeTag: Some[TypeTag[Boolean]] = Some(implicitly[TypeTag[Boolean]])
  override def parse(s: String): ParserResult[Boolean] = s match {
    case "true" => ParserSuccess(true)
    case "false" => ParserSuccess(false)
    case _ => StringParser.invalidBooleanFormat
  }
}

class DoubleParser extends StringParser[Double] {
  override val typeTag: Some[TypeTag[Double]] = Some(implicitly[TypeTag[Double]])
  override def parse(s: String): ParserResult[Double] =
    try ParserSuccess(s.toDouble)
    catch { case e: NumberFormatException => StringParser.invalidNumberFormat }
}

class FloatParser extends StringParser[Float] {
  override val typeTag: Some[TypeTag[Float]] = Some(implicitly[TypeTag[Float]])
  override def parse(s: String): ParserResult[Float] =
    try ParserSuccess(s.toFloat)
    catch { case e: NumberFormatException => StringParser.invalidNumberFormat }
}

class IntParser extends StringParser[Int] {
  override val typeTag: Some[TypeTag[Int]] = Some(implicitly[TypeTag[Int]])
  override def parse(s: String): ParserResult[Int] =
    try ParserSuccess(s.toInt)
    catch { case e: NumberFormatException => StringParser.invalidNumberFormat }
}

class LongParser extends StringParser[Long] {
  override val typeTag: Some[TypeTag[Long]] = Some(implicitly[TypeTag[Long]])
  override def parse(s: String): ParserResult[Long] =
    try ParserSuccess(s.toLong)
    catch { case e: NumberFormatException => StringParser.invalidNumberFormat }
}

class ShortParser extends StringParser[Short] {
  override val typeTag: Some[TypeTag[Short]] = Some(implicitly[TypeTag[Short]])
  override def parse(s: String): ParserResult[Short] =
    try ParserSuccess(s.toShort)
    catch { case e: NumberFormatException => StringParser.invalidNumberFormat }
}

object StringParser {

  ////////////////////// Default parsers //////////////////////////////

  implicit val booleanParser = new BooleanParser
  implicit val doubleParser = new DoubleParser
  implicit val floatParser = new FloatParser
  implicit val intParser = new IntParser
  implicit val longParser = new LongParser
  implicit val shortParser = new ShortParser

  implicit val strParser = new StringParser[String] {
    override val typeTag: Some[TypeTag[String]] = Some(implicitly[TypeTag[String]])
    override def parse(s: String): ParserResult[String] = ParserSuccess(s)
  }

  private[bits] val invalidNumberFormat = ParserFailure.badRequest("Invalid number format.")
  private[bits] val invalidBooleanFormat = ParserFailure.badRequest("Invalid Boolean Format. Use 'true' or 'false'")

}
