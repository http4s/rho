package org.http4s.rho.swagger

import java.sql.Timestamp
import java.util.Date

object Reflector {
  import scala.reflect.runtime.universe._

  private[swagger] val primitives =
    Set[Type](
      typeOf[String],
      typeOf[Int],
      typeOf[Long],
      typeOf[Double],
      typeOf[Float],
      typeOf[Byte],
      typeOf[BigInt],
      typeOf[Boolean],
      typeOf[Short],
      typeOf[Char],
      typeOf[java.lang.Integer],
      typeOf[java.lang.Long],
      typeOf[java.lang.Double],
      typeOf[java.lang.Float],
      typeOf[BigDecimal],
      typeOf[java.lang.Byte],
      typeOf[java.lang.Boolean],
      typeOf[java.lang.Number],
      typeOf[java.lang.Short],
      typeOf[Date],
      typeOf[Timestamp],
      typeOf[scala.Symbol],
      typeOf[java.math.BigDecimal],
      typeOf[java.math.BigInteger],
      typeOf[java.lang.Character]
    )

  private[swagger] val excludes =
    Set[Type](typeOf[scala.xml.Elem])

  def isPrimitive(t: Type, extra: Set[Type] = Set.empty) = (primitives ++ extra).exists(t =:= _)

  def isExcluded(t: Type, extra: Set[Type] = Set.empty) = (excludes ++ extra).exists(t <:< _)
}
