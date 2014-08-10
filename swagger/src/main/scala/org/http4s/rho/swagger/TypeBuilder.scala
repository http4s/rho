/*  This code was largely pillaged from worked by Wordnik, which is under an Apache 2.0 license.
    https://github.com/swagger-api/swagger-scala.git
    commit: 5746ffcc44a99dfaf25c41444559bb2bd197a33a
 */

// TODO: deal with Maps correctly

package org.http4s.rho.swagger

import java.sql.Timestamp
import java.util.Date

import com.typesafe.scalalogging.slf4j.StrictLogging
import com.wordnik.swagger.annotations.{ApiModel, ApiModelProperty}
import com.wordnik.swagger.model._
import org.http4s.DateTime
import org.joda.time.{DateTimeZone, Chronology, ReadableInstant}

import scala.collection.mutable.LinkedHashMap
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

import scalaz.stream.Process
import scalaz.concurrent.Task

object TypeBuilder extends StrictLogging {

  implicit class ReflectionHelpers(t: Type) {
    import scala.reflect.runtime.universe._
    def simpleName: String = {
      t.typeSymbol.name.decodedName.toString + {
        if (t.typeArgs.isEmpty) ""
        else t.typeArgs.map(_.simpleName).mkString("[", ",", "]")
      }
    }

    def fullName: String = {
      t.typeSymbol.fullName + {
        if (t.typeArgs.isEmpty) ""
        else t.typeArgs.map(_.fullName).mkString("[", ",", "]")
      }
    }

    def isProcess: Boolean = t <:< typeOf[Process[Task,_]]
    def isMap: Boolean = t <:< typeOf[collection.immutable.Map[_, _]] || t <:< typeOf[collection.Map[_, _]]
    def isCollection: Boolean = t <:< typeOf[Array[_]] ||
                                t <:< typeOf[Iterable[_]] ||
                                t <:< typeOf[java.util.Collection[_]]

    def isPrimitive: Boolean = Reflector.primitives.find(_ =:= t).isDefined ||
                               Reflector.isPrimitive(t, Set(typeOf[Char], typeOf[Unit]))

    def isOption: Boolean = t <:< typeOf[Option[_]]
    def isEither: Boolean = t <:< typeOf[Either[_, _]]
    def isArray: Boolean = t <:< typeOf[Array[_]]

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

    def isPrimitive(t: Type, extra: Set[Type] = Set.empty) = (primitives ++ extra).exists(t =:= _)
  }


  ///////////////////////////////////////////////////////////////////////////////////

  val baseTypes = Set("byte", "boolean", "int", "long", "float", "double", "string", "date", "void", "Date", "DateTime", "DateMidnight", "Duration", "FiniteDuration", "Chronology")
  val excludes: Set[Type] = Set(typeOf[java.util.TimeZone] ,typeOf[java.util.Date], typeOf[DateTime], typeOf[ReadableInstant], typeOf[Chronology], typeOf[DateTimeZone])
  val containerTypes = Set("Array", "List", "Set")

  def collectModels(t: TypeTag[_], alreadyKnown: Set[Model]): Set[Model] =
    try collectModels(t.tpe.dealias, alreadyKnown, Set.empty)
    catch { case NonFatal(e) => logger.error(s"Failed to build model for type: ${t.tpe.fullName}", e); Set.empty}

  private def collectModels(tpe: Type, alreadyKnown: Set[Model], known: Set[Type]): Set[Model] = {
    if (tpe.isMap) {
      collectModels(tpe.typeArgs.head, alreadyKnown, tpe.typeArgs.toSet) ++
        collectModels(tpe.typeArgs.last, alreadyKnown, tpe.typeArgs.toSet)
    }
    else if (tpe.isCollection || tpe.isOption) {
      val ntpe = tpe.typeArgs.head
      if (! known.exists(_ =:= ntpe)) collectModels(ntpe, alreadyKnown, known + ntpe)
      else Set.empty
    }
    else if (tpe.isProcess) {
      val ntpe = tpe.typeArgs.apply(1)
      if (! known.exists(_ =:= ntpe)) collectModels(ntpe, alreadyKnown, known + ntpe)
      else Set.empty
    }
    else if (alreadyKnown.map(_.id).contains(tpe.simpleName) ||(tpe.isPrimitive)) Set.empty  // Not a map or collection

    else {
      val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
      if (sym.isClass && sym.asClass.isCaseClass && sym.asClass.primaryConstructor.isMethod) {
        val ctor = sym.asClass.primaryConstructor.asMethod

        val models = alreadyKnown ++ modelToSwagger(tpe)

        val children = ctor.paramLists.flatten.flatMap { paramsym =>
          val pTpe = if (sym.isClass) paramsym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
                     else sym.typeSignature
          collectModels(pTpe, alreadyKnown, known + tpe)
        }

        models ++ children
      }
      else {
        logger.warn(s"TypeBuilder cannot describe types other than case classes. Failing type: ${tpe.fullName}")
        Set.empty
      }
    }
  }

  private[this] val defaultExcluded = Set(typeOf[Nothing], typeOf[Null])
  private[this] def isExcluded(t: Type, excludes: Seq[Type] = Nil) = (defaultExcluded ++ excludes).exists(_ =:= t)

  def modelToSwagger(tpe: Type): Option[Model] = try {
    val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe

    val properties: Seq[(String, ModelProperty)] =
      tpe.member(termNames.CONSTRUCTOR)
         .typeSignature
         .paramLists
         .flatten
         .zipWithIndex
         .map { case (pSym, pos) =>
           val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)

           val items: Option[ModelRef] = {
             if (pTpe.isCollection) {
               val t = pTpe.typeArgs.head

               val m = ModelRef(`type` = DataType.fromType(t).name,
                                qualifiedType = Some(t.fullName)
                               )
               Some(m)
             }
             else None
           }

           val prop = ModelProperty(
             DataType.fromType(pTpe).name,
             pTpe.fullName,
             pos,
             !(pSym.asTerm.isParamWithDefault || pTpe.isOption),
             items = items
           )

           (pSym.name.decodedName.toString, prop)
         }

    val m = Model(tpe.simpleName, tpe.simpleName, tpe.fullName, LinkedHashMap(properties: _*))
    Some(m)
  } catch { case NonFatal(t) => logger.error("Failed to build Swagger model", t); None }

//  object AllowableValues {
//    case object AnyValue extends AllowableValues
//    case class AllowableValuesList[T](values: List[T]) extends AllowableValues
//    case class AllowableRangeValues(values: Range) extends AllowableValues
//
//    def apply(): AllowableValues = empty
//    def apply[T](values: T*): AllowableValues = apply(values.toList)
//    def apply[T](values: List[T]): AllowableValues = AllowableValuesList(values)
//    def apply(values: Range): AllowableValues = AllowableRangeValues(values)
//    def empty = AnyValue
//  }
//
//  private def convertToAllowableValues(csvString: String, paramType: String = null): AllowableValues = {
//    if (csvString.toLowerCase.startsWith("range[")) {
//      val ranges = csvString.substring(6, csvString.length() - 1).split(",")
//      buildAllowableRangeValues(ranges, csvString, inclusive = true)
//    } else if (csvString.toLowerCase.startsWith("rangeexclusive[")) {
//      val ranges = csvString.substring(15, csvString.length() - 1).split(",")
//      buildAllowableRangeValues(ranges, csvString, inclusive = false)
//    } else {
//      if (csvString.isBlank) {
//        AllowableValues.AnyValue
//      } else {
//        val params = csvString.split(",").toList
//        //        implicit val format = DefaultJsonFormats.GenericFormat(DefaultReaders.StringReader, DefaultWriters.StringWriter)
//        paramType match {
//          case null => AllowableValues.AllowableValuesList(params)
//          case "string" => AllowableValues.AllowableValuesList(params)
//        }
//      }
//    }
//  }
//
//  private def buildAllowableRangeValues(ranges: Array[String], inputStr: String, inclusive: Boolean = true): AllowableValues.AllowableRangeValues = {
//    var min: java.lang.Float = 0
//    var max: java.lang.Float = 0
//    if (ranges.size < 2) {
//      throw new RuntimeException("Allowable values format " + inputStr + "is incorrect")
//    }
//    if (ranges(0).equalsIgnoreCase("Infinity")) {
//      min = Float.PositiveInfinity
//    } else if (ranges(0).equalsIgnoreCase("-Infinity")) {
//      min = Float.NegativeInfinity
//    } else {
//      min = ranges(0).toFloat
//    }
//    if (ranges(1).equalsIgnoreCase("Infinity")) {
//      max = Float.PositiveInfinity
//    } else if (ranges(1).equalsIgnoreCase("-Infinity")) {
//      max = Float.NegativeInfinity
//    } else {
//      max = ranges(1).toFloat
//    }
//    val allowableValues =
//      AllowableValues.AllowableRangeValues(if (inclusive) Range.inclusive(min.toInt, max.toInt) else Range(min.toInt, max.toInt))
//    allowableValues
//  }
//
//  private implicit class RichString(str: String) {
//    def blankOption: Option[String] = if (isBlank) None else Some(str)
//    def isBlank: Boolean = str == null || str.isEmpty
//  }
//
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

    //  object GenMap {
    //    def apply(): DataType = Map
    //    def apply(k: DataType, v: DataType): DataType = new DataType("Map[%s, %s]" format(k.name, v.name))
    //  }
    //

    // Methods to get a Datatype
    def apply(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) =
      new ValueDataType(name, format, qualifiedName)

    def apply(tag: TypeTag[_]): DataType = fromType(tag.tpe.dealias)

    private[this] val StringTypes = Set[Type](typeOf[String],typeOf[java.lang.String])
    private[this] def isString(t: Type) = StringTypes.exists(t =:= _)
    private[this] val BoolTypes = Set[Type](typeOf[Boolean],typeOf[java.lang.Boolean])
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
      //    else if (classOf[java.lang.Enum[_]].isAssignableFrom(klass)) this.Enum
      //    else if (isMap(klass)) {
      //      if (st.typeArgs.size == 2) {
      //        val (k :: v :: Nil) = st.typeArgs.toList
      //        GenMap(fromScalaType(k), fromScalaType(v))
      //      } else GenMap()
      //    }
      else if (klass <:< typeOf[scala.collection.Set[_]] || klass <:< typeOf[java.util.Set[_]]) {
        if (t.typeArgs.nonEmpty) GenSet(fromType(t.typeArgs.head))
        else GenSet()
      }
      else if (klass <:< typeOf[collection.Seq[_]] || klass <:< typeOf[java.util.List[_]]) {
        if (t.typeArgs.nonEmpty) GenList(fromType(t.typeArgs.head))
        else GenList()
      }
      else if (t.isArray || isCollection(klass)) {
        if (t.typeArgs.nonEmpty) GenArray(fromType(t.typeArgs.head))
        else GenArray()
      }
      else if (t.isProcess) {
        if (t.typeArgs.nonEmpty) GenArray(fromType(t.typeArgs(1)))
        else GenArray()
      }
      else {
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
