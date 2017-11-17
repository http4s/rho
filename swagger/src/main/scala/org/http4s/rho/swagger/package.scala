package org.http4s.rho

import java.sql.Timestamp
import java.util.Date

import org.http4s.Method
import org.http4s.rho.bits.{PathAST, SecurityScopesMetaData, TextMetaData}
import org.http4s.rho.bits.ResponseGenerator.EmptyRe
import org.http4s.rho.swagger.models.Model
import shapeless.{HList, HNil}

import scala.reflect.runtime.universe._
import fs2.{Task, Stream}

package object swagger {

  /** Metadata carrier for specific routes */
  case class RouteDesc(msg: String) extends TextMetaData

  /** Add support for adding documentation before a route using the ** operator */
  implicit class StrOps(description: String) {
    def **(method: Method): PathBuilder[HNil] = **(new PathBuilder[HNil](method, PathEmpty))

    def **[T<: HNil](builder: PathBuilder[T]): PathBuilder[T] =
      new PathBuilder(builder.method, PathAST.MetaCons(builder.path, RouteDesc(description)))
  }

  /** Scopes carrier for specific routes */
  case class RouteSecurityScope(definitions: Map[String, List[String]]) extends SecurityScopesMetaData

  /** Add support for adding documentation before a route using the ## operator */
  implicit class SecOps(definitions: Map[String, List[String]]) {
    def ^^(method: Method): PathBuilder[HNil] = ^^(new PathBuilder[HNil](method, PathEmpty))

    def ^^(route: RhoRoute.Tpe): PathBuilder[HNil] = new PathBuilder(route.method, PathAST.MetaCons(route.path, RouteSecurityScope(definitions)))

    def ^^[T<: HList](builder: PathBuilder[T]): PathBuilder[T] =
      new PathBuilder(builder.method, PathAST.MetaCons(builder.path, RouteSecurityScope(definitions)))

  }

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

    private[swagger] val excludes = {
      Set[Type](typeOf[scala.xml.Elem], typeOf[EmptyRe])
    }

    def isPrimitive(t: Type, extra: Set[Type] = Set.empty) = (primitives ++ extra).exists(t =:= _)

    def isExcluded(t: Type, extra: Set[Type] = Set.empty) = (excludes ++ extra).exists(t <:< _)
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
      Reflector.primitives.exists(_ =:= t) ||
        Reflector.isPrimitive(t, Set(typeOf[Char], typeOf[Unit]))

    def isStream: Boolean =
      t <:< typeOf[Stream[Task, _]]

    def isTask: Boolean =
      t <:< typeOf[Task[_]]

    def isSwaggerFile: Boolean =
      t <:< typeOf[SwaggerFileResponse[_]]
  }

  val DefaultSwaggerFormats: SwaggerFormats = {
    val ignoreExistentialType: PartialFunction[Type, Set[Model]] = {
      case ExistentialType(_, _) => Set.empty
    }

    val ignoreNothingOrNull: PartialFunction[Type, Set[Model]] = {
      case tpe if tpe.isNothingOrNull => Set.empty
    }

    SwaggerFormats(
      ignoreNothingOrNull orElse ignoreExistentialType,
      SwaggerFormats.emptyFieldSerializers
    )
  }

  val EmptySwaggerFormats: SwaggerFormats =
    SwaggerFormats(SwaggerFormats.emptySerializers, SwaggerFormats.emptyFieldSerializers)

}
