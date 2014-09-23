package org.http4s.rho

import java.sql.Timestamp
import java.util.Date
import scala.reflect.runtime.universe._
import scalaz.stream.Process
import scalaz.concurrent.Task
import scala.annotation.implicitNotFound
import com.wordnik.swagger.model.ModelProperty
import com.wordnik.swagger.model.Model

package object swagger {

  object Reflector {
    import scala.reflect.runtime.universe._

    private[swagger] val primitives = {
      Set[Type](typeOf[String], typeOf[Int], typeOf[Long], typeOf[Double],
        typeOf[Float], typeOf[Byte], typeOf[BigInt], typeOf[Boolean],
        typeOf[Short], typeOf[java.lang.Integer], typeOf[java.lang.Long],
        typeOf[java.lang.Double], typeOf[java.lang.Float], typeOf[BigDecimal],
        typeOf[java.lang.Byte], typeOf[java.lang.Boolean], typeOf[Number],
        typeOf[java.lang.Short], typeOf[Date], typeOf[Timestamp], typeOf[scala.Symbol],
        typeOf[java.math.BigDecimal], typeOf[java.math.BigInteger])
    }

    def isPrimitive(t: Type, extra: Set[Type] = Set.empty) = (primitives ++ extra).exists(t =:= _)
  }

  implicit class ReflectionHelpers(t: Type) {
    import scala.reflect.runtime.universe._

    val genericStart = "«"
    val genericSep = ","
    val genericEnd = "»"

    def simpleName: String = {
      t.typeSymbol.name.decodedName.toString + {
        if (t.typeArgs.isEmpty) ""
        else t.typeArgs.map(_.simpleName).mkString(genericStart, genericSep, genericEnd)
      }
    }

    def fullName: String = {
      t.typeSymbol.fullName + {
        if (t.typeArgs.isEmpty) ""
        else t.typeArgs.map(_.fullName).mkString(genericStart, genericSep, genericEnd)
      }
    }

    def isArray: Boolean =
      t <:< typeOf[Array[_]]

    def isCollection: Boolean = t <:< typeOf[Array[_]] ||
      t <:< typeOf[Iterable[_]] ||
      t <:< typeOf[java.util.Collection[_]]

    def isEither: Boolean =
      t <:< typeOf[Either[_, _]]

    def isMap: Boolean =
      t <:< typeOf[collection.immutable.Map[_, _]] || t <:< typeOf[collection.Map[_, _]]

    def isNothingOrNull: Boolean =
      t <:< typeOf[Nothing] || t <:< typeOf[Null]

    def isOption: Boolean =
      t <:< typeOf[Option[_]]

    def isPrimitive: Boolean =
      Reflector.primitives.find(_ =:= t).isDefined ||
        Reflector.isPrimitive(t, Set(typeOf[Char], typeOf[Unit]))

    def isProcess: Boolean =
      t <:< typeOf[Process[Task, _]]

  }

}
