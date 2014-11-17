package org.http4s.rho.swagger

import java.util.Date

import com.wordnik.swagger.model._

import org.http4s.DateTime
import org.joda.time.{ DateTimeZone, Chronology, ReadableInstant }

import scala.collection.mutable.LinkedHashMap
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

import org.log4s.getLogger

object TypeBuilder {

  private[this] val logger = getLogger

  val baseTypes = Set("byte", "boolean", "int", "long", "float", "double", "string", "date", "void", "Date", "DateTime", "DateMidnight", "Duration", "FiniteDuration", "Chronology")
  val excludes: Set[Type] = Set(typeOf[java.util.TimeZone], typeOf[java.util.Date], typeOf[DateTime], typeOf[ReadableInstant], typeOf[Chronology], typeOf[DateTimeZone])
  val containerTypes = Set("Array", "List", "Set")

  def collectModels(t: Type, alreadyKnown: Set[Model], formats: SwaggerFormats): Set[Model] =
    try collectModels(t.dealias, alreadyKnown, Set.empty, formats)
    catch { case NonFatal(e) => logger.error(e)(s"Failed to build model for type: ${t.fullName}"); Set.empty }

  private def collectModels(t: Type, alreadyKnown: Set[Model], known: Set[Type], formats: SwaggerFormats): Set[Model] = {
    def go(t: Type, alreadyKnown: Set[Model], known: Set[Type]): Set[Model] = t.dealias match {
      // apply custom serializers first
      case tpe if formats.customSerializers.isDefinedAt(tpe) =>
        formats.customSerializers(tpe)

      // TODO it would be the best if we could pull out the following cases into DefaultFormats
      case tpe if tpe.isNothingOrNull =>
        Set.empty
      case tpe if tpe.isEither || tpe.isMap =>
        go(tpe.typeArgs.head, alreadyKnown, tpe.typeArgs.toSet) ++
          go(tpe.typeArgs.last, alreadyKnown, tpe.typeArgs.toSet)
      case tpe if (tpe.isCollection || tpe.isOption) && tpe.typeArgs.nonEmpty =>
        val ntpe = tpe.typeArgs.head
        if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
        else Set.empty
      case tpe if tpe.isProcess =>
        val ntpe = tpe.typeArgs.apply(1)
        if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
        else Set.empty
      case tpe if tpe.isTask =>
        val ntpe = tpe.typeArgs.apply(0)
        if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
        else Set.empty
      case tpe if (alreadyKnown.map(_.id).contains(tpe.simpleName) || (tpe.isPrimitive)) =>
        Set.empty
      case ExistentialType(_, _) =>
        Set.empty
      case tpe@TypeRef(_, sym: Symbol, tpeArgs: List[Type]) if isCaseClass(sym) =>
        val ctor = sym.asClass.primaryConstructor.asMethod
        val models = alreadyKnown ++ modelToSwagger(tpe)
        val generics = tpe.typeArgs.foldLeft(List[Model]()) { (acc, t) =>
          acc ++ go(t, alreadyKnown, tpe.typeArgs.toSet)
        }
        val children = ctor.paramLists.flatten.flatMap { paramsym =>
          val paramType = if (sym.isClass) paramsym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
          else sym.typeSignature
          go(paramType, alreadyKnown, known + tpe)
        }

        models ++ generics ++ children
      case e =>
        logger.warn(s"TypeBuilder failed to build type. Failing type: ${e.fullName}")
        Set.empty
    }

    go(t, alreadyKnown, known)
  }

  private[this] val defaultExcluded = Set(typeOf[Nothing], typeOf[Null])
  private[this] def isCaseClass(sym: Symbol): Boolean = sym.isClass && sym.asClass.isCaseClass && sym.asClass.primaryConstructor.isMethod
  private[this] def isExcluded(t: Type, excludes: Seq[Type] = Nil) = (defaultExcluded ++ excludes).exists(_ =:= t)

  def modelToSwagger(tpe: Type): Option[Model] = try {
    val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe

    val properties: Seq[(String, ModelProperty)] =
      tpe.member(termNames.CONSTRUCTOR)
        .typeSignature
        .paramLists
        .flatten
        .zipWithIndex
        .map {
          case (paramSymbol, pos) =>
            val paramType = paramSymbol.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)

            val items: Option[ModelRef] = {
              if (paramType.isCollection && !paramType.isNothingOrNull) {
                val t = paramType.dealias.typeArgs.head

                val m = ModelRef(`type` = DataType.fromType(t).name,
                  qualifiedType = Some(t.fullName))
                Some(m)
              } else None
            }

            val prop = ModelProperty(
              DataType.fromType(paramType).name,
              paramType.fullName,
              pos,
              !(paramSymbol.asTerm.isParamWithDefault || paramType.isOption),
              items = items)

            (paramSymbol.name.decodedName.toString, prop)
        }

    val m = Model(tpe.simpleName, tpe.simpleName, tpe.fullName, LinkedHashMap(properties: _*))
    Some(m)
  } catch { case NonFatal(t) => logger.error(t)("Failed to build Swagger model"); None }

  sealed trait DataType {
    def name: String
  }

  object DataType {

    case class ValueDataType(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) extends DataType
    case class ContainerDataType(name: String, typeArg: Option[DataType] = None, uniqueItems: Boolean = false) extends DataType

    val Void = DataType("void")
    val String = DataType("string")
    val Byte = DataType("string", Some("byte"))
    val Int = DataType("integer", Some("int32"))
    val Long = DataType("integer", Some("int64"))
    val Float = DataType("number", Some("float"))
    val Double = DataType("number", Some("double"))
    val Boolean = DataType("boolean")
    val Date = DataType("string", Some("date"))
    val DateTime = DataType("string", Some("date-time"))

    object GenList {
      def apply(): DataType = ContainerDataType("List")
      def apply(v: DataType): DataType = new ContainerDataType("List", Some(v))
    }

    object GenSet {
      def apply(): DataType = ContainerDataType("Set", uniqueItems = true)
      def apply(v: DataType): DataType = new ContainerDataType("Set", Some(v), uniqueItems = true)
    }

    object GenArray {
      def apply(): DataType = ContainerDataType("Array")
      def apply(v: DataType): DataType = new ContainerDataType("Array", Some(v))
    }

    // Methods to get a Datatype
    def apply(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) =
      new ValueDataType(name, format, qualifiedName)

    def apply(tag: TypeTag[_]): DataType = apply(tag.tpe)
    def apply(tag: Type): DataType = fromType(tag.dealias)

    private[this] val StringTypes = Set[Type](typeOf[String], typeOf[java.lang.String])
    private[this] def isString(t: Type) = StringTypes.exists(t =:= _)
    private[this] val BoolTypes = Set[Type](typeOf[Boolean], typeOf[java.lang.Boolean])
    private[this] def isBool(t: Type) = BoolTypes.exists(t =:= _)

    private[swagger] def fromType(t: Type): DataType = {
      val klass = if (t.isOption && t.typeArgs.size > 0) t.typeArgs.head else t
      if (klass <:< typeOf[Unit] || klass <:< typeOf[Void]) this.Void
      else if (isString(klass)) this.String
      else if (klass <:< typeOf[Byte] || klass <:< typeOf[java.lang.Byte]) this.Byte
      else if (klass <:< typeOf[Long] || klass <:< typeOf[java.lang.Long]) this.Long
      else if (isInt(klass)) this.Int
      else if (klass <:< typeOf[Float] || klass <:< typeOf[java.lang.Float]) this.Float
      else if (isDecimal(klass)) this.Double
      else if (isDateTime(klass)) this.DateTime
      else if (isBool(klass)) this.Boolean
      else if (klass <:< typeOf[scala.collection.Set[_]] || klass <:< typeOf[java.util.Set[_]]) {
        if (t.typeArgs.nonEmpty) GenSet(fromType(t.typeArgs.head))
        else GenSet()
      } else if (klass <:< typeOf[collection.Seq[_]] || klass <:< typeOf[java.util.List[_]]) {
        if (t.typeArgs.nonEmpty) GenList(fromType(t.typeArgs.head))
        else GenList()
      } else if (t.isArray || isCollection(klass)) {
        if (t.typeArgs.nonEmpty) GenArray(fromType(t.typeArgs.head))
        else GenArray()
      } else if (t.isProcess) {
        if (t.typeArgs.nonEmpty) GenArray(fromType(t.typeArgs(1)))
        else GenArray()
      } else {
        val stt = if (t.isOption) t.typeArgs.head else t
        new ValueDataType(stt.simpleName, qualifiedName = Option(stt.fullName))
      }
    }

    private[this] val IntTypes =
      Set[Type](typeOf[Int], typeOf[java.lang.Integer], typeOf[Short], typeOf[java.lang.Short], typeOf[BigInt], typeOf[java.math.BigInteger])
    private[this] def isInt(t: Type): Boolean = IntTypes.exists(t =:= _)

    private[this] val DecimalTypes =
      Set[Type](typeOf[Double], typeOf[java.lang.Double], typeOf[BigDecimal], typeOf[java.math.BigDecimal])
    private[this] def isDecimal(t: Type): Boolean = DecimalTypes.exists(t =:= _)

    private[this] val DateTimeTypes =
      Set[Type](typeOf[Date], typeOf[DateTime])
    private[this] def isDateTime(t: Type): Boolean = DateTimeTypes.exists(t <:< _)

    private[this] def isCollection(t: Type): Boolean =
      t <:< typeOf[collection.Traversable[_]] ||
        t <:< typeOf[java.util.Collection[_]]

  }
}
