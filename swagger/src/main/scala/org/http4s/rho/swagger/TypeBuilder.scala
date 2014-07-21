/*  This code was largely pillaged from worked by Wordnik, which is under an Apache 2.0 license.
    https://github.com/swagger-api/swagger-scala.git
    commit: 5746ffcc44a99dfaf25c41444559bb2bd197a33a
 */

package org.http4s.rho.swagger

import java.util.Date
import java.lang.reflect.{Field,Type}

import com.wordnik.swagger.annotations.{ApiModel, ApiModelProperty}
import com.wordnik.swagger.model.{AllowableValues, ModelProperty, Model}
import org.http4s.DateTime
import org.joda.time.{DateTimeZone, Chronology, ReadableInstant}
import org.json4s.reflect.{PropertyDescriptor, ClassDescriptor, ScalaType, Reflector}

import scala.reflect.runtime.universe.TypeTag

import scala.collection.mutable

object TypeBuilder {
  val baseTypes = Set("byte", "boolean", "int", "long", "float", "double", "string", "date", "void", "Date", "DateTime", "DateMidnight", "Duration", "FiniteDuration", "Chronology")
  val excludes: Set[java.lang.reflect.Type] = Set(classOf[java.util.TimeZone] ,classOf[java.util.Date], classOf[DateTime], classOf[ReadableInstant], classOf[Chronology], classOf[DateTimeZone])
  val containerTypes = Set("Array", "List", "Set")

  def collectModels(t: TypeTag[_], alreadyKnown: Set[Model]): Set[Model] = collectModels(alreadyKnown)(tagToManifest(t))

  private def collectModels[T: Manifest](alreadyKnown: Set[Model]): Set[Model] = collectModels(Reflector.scalaTypeOf[T], alreadyKnown)

  private def collectModels(tpe: ScalaType, alreadyKnown: Set[Model], known: Set[ScalaType] = Set.empty): Set[Model] = {
    if (tpe.isMap) collectModels(tpe.typeArgs.head, alreadyKnown, tpe.typeArgs.toSet) ++ collectModels(tpe.typeArgs.last, alreadyKnown, tpe.typeArgs.toSet)
    else if (tpe.isCollection || tpe.isOption) {
      val ntpe = tpe.typeArgs.head
      if (! known.contains(ntpe)) collectModels(ntpe, alreadyKnown, known + ntpe)
      else Set.empty
    }
    else {
      if (alreadyKnown.map(_.id).contains(tpe.simpleName) || Reflector.isPrimitive(tpe.erasure, Set(classOf[Char], classOf[Unit]))) Set.empty
      else {
        val descr = Reflector.describe(tpe)
        descr match {
          case descriptor: ClassDescriptor =>
            val ctorModels = descriptor.mostComprehensive.filterNot(_.argType.isPrimitive).toVector
            val propModels = descriptor.properties.filterNot(p => p.returnType.isPrimitive || ctorModels.exists(_.name == p.name))
            val subModels = (ctorModels.map(_.argType) ++ propModels.map(_.returnType)).toSet -- known
            val topLevel = for {
              tl <- subModels + descriptor.erasure
              if  !(tl.isCollection || tl.isMap || tl.isOption)
              m <- modelToSwagger(tl)
            } yield m

            val nested = subModels.foldLeft((topLevel, known + descriptor.erasure)){ (acc, b) =>
              val m = collectModels(b, alreadyKnown, acc._2)
              (acc._1 ++ m, acc._2 + b)
            }
            nested._1
          case _ => Set.empty
        }
      }
    }
  }

  def modelToSwagger[T](implicit mf: Manifest[T]): Option[Model] = modelToSwagger(Reflector.scalaTypeOf[T])

  private[this] def toModelProperty(descr: ClassDescriptor, position: Option[Int] = None, required: Boolean = true, description: Option[String] = None, allowableValues: String = "")(prop: PropertyDescriptor) = {
    val ctorParam = if (!prop.returnType.isOption) descr.mostComprehensive.find(_.name == prop.name) else None
    //    if (descr.simpleName == "Pet") println("converting property: " + prop)
    val mp = ModelProperty(
      DataType.fromScalaType(if (prop.returnType.isOption) prop.returnType.typeArgs.head else prop.returnType).name,
      descr.fullName,
      if (position.isDefined && position.forall(_ >= 0)) position.get else ctorParam.map(_.argIndex).getOrElse(position.getOrElse(0)),
      required = required && !prop.returnType.isOption,
      description = description.flatMap(_.blankOption),
      allowableValues = convertToAllowableValues(allowableValues))
    //    if (descr.simpleName == "Pet") println("The property is: " + mp)
    prop.name -> mp
  }

  private[this] val defaultExcluded = Set(classOf[Nothing], classOf[Null])
  private[this] def isExcluded(t: Type, excludes: Seq[Type] = Nil) = (defaultExcluded ++ excludes) contains t

  def modelToSwagger(klass: ScalaType): Option[Model] = {
    if (Reflector.isPrimitive(klass.erasure) || isExcluded(klass.erasure, excludes.toSeq)) None
    else {
      val name = klass.simpleName

      val descr = Reflector.describe(klass).asInstanceOf[ClassDescriptor]
      val apiModel = Option(klass.erasure.getAnnotation(classOf[ApiModel]))

      val fields = klass.erasure.getDeclaredFields.toList map {
        case f: Field =>
          val asModelProperty = if (f.getAnnotation(classOf[ApiModelProperty]) != null) {
            val annot = f.getAnnotation(classOf[ApiModelProperty])
            toModelProperty(descr, Some(annot.position()), annot.required(), annot.notes().blankOption, annot.allowableValues())_
          } else {
//            val annot = f.getAnnotation(classOf[ApiEnum])
//            toModelProperty(descr, allowableValues = if (annot != null) annot.values().mkString(",") else "")_
            toModelProperty(descr, allowableValues = "")_
          }
          descr.properties.find(_.mangledName == f.getName) map asModelProperty

      }

      val fieldsMap = {
        val map = new mutable.LinkedHashMap[String, ModelProperty]
        fields.flatten.foreach(map += _)
        map
      }

      val result = apiModel map { am =>
        val fieldsMap = new mutable.LinkedHashMap[String, ModelProperty]
        fields.flatten.foreach(fieldsMap += _)
        Model(name, name, klass.fullName.blankOption.getOrElse(""), properties = fieldsMap, baseModel = am.parent.getName.blankOption, discriminator = am.discriminator.blankOption )
      } orElse Some(Model(name, name, klass.fullName.blankOption.getOrElse(""), properties = fieldsMap))
      //      if (descr.simpleName == "Pet") println("The collected fields:\n" + result)
      result
    }
  }

  object AllowableValues {
    case object AnyValue extends AllowableValues
    case class AllowableValuesList[T](values: List[T]) extends AllowableValues
    case class AllowableRangeValues(values: Range) extends AllowableValues

    def apply(): AllowableValues = empty
    def apply[T](values: T*): AllowableValues = apply(values.toList)
    def apply[T](values: List[T]): AllowableValues = AllowableValuesList(values)
    def apply(values: Range): AllowableValues = AllowableRangeValues(values)
    def empty = AnyValue
  }

  private def convertToAllowableValues(csvString: String, paramType: String = null): AllowableValues = {
    if (csvString.toLowerCase.startsWith("range[")) {
      val ranges = csvString.substring(6, csvString.length() - 1).split(",")
      buildAllowableRangeValues(ranges, csvString, inclusive = true)
    } else if (csvString.toLowerCase.startsWith("rangeexclusive[")) {
      val ranges = csvString.substring(15, csvString.length() - 1).split(",")
      buildAllowableRangeValues(ranges, csvString, inclusive = false)
    } else {
      if (csvString.isBlank) {
        AllowableValues.AnyValue
      } else {
        val params = csvString.split(",").toList
        //        implicit val format = DefaultJsonFormats.GenericFormat(DefaultReaders.StringReader, DefaultWriters.StringWriter)
        paramType match {
          case null => AllowableValues.AllowableValuesList(params)
          case "string" => AllowableValues.AllowableValuesList(params)
        }
      }
    }
  }

  private def buildAllowableRangeValues(ranges: Array[String], inputStr: String, inclusive: Boolean = true): AllowableValues.AllowableRangeValues = {
    var min: java.lang.Float = 0
    var max: java.lang.Float = 0
    if (ranges.size < 2) {
      throw new RuntimeException("Allowable values format " + inputStr + "is incorrect")
    }
    if (ranges(0).equalsIgnoreCase("Infinity")) {
      min = Float.PositiveInfinity
    } else if (ranges(0).equalsIgnoreCase("-Infinity")) {
      min = Float.NegativeInfinity
    } else {
      min = ranges(0).toFloat
    }
    if (ranges(1).equalsIgnoreCase("Infinity")) {
      max = Float.PositiveInfinity
    } else if (ranges(1).equalsIgnoreCase("-Infinity")) {
      max = Float.NegativeInfinity
    } else {
      max = ranges(1).toFloat
    }
    val allowableValues =
      AllowableValues.AllowableRangeValues(if (inclusive) Range.inclusive(min.toInt, max.toInt) else Range(min.toInt, max.toInt))
    allowableValues
  }

  private implicit class RichString(str: String) {
    def blankOption: Option[String] = if (isBlank) None else Some(str)
    def isBlank: Boolean = str == null || str.isEmpty
  }

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

    def apply(tag: TypeTag[_]): DataType = apply(tagToManifest(tag))
    def apply[T](implicit mf: Manifest[T]): DataType = fromManifest[T](mf)

    private[this] val StringTypes = Set[Class[_]](classOf[String],classOf[java.lang.String])
    private[this] def isString(klass: Class[_]) = StringTypes contains klass
    private[this] val BoolTypes = Set[Class[_]](classOf[Boolean],classOf[java.lang.Boolean])
    private[this] def isBool(klass: Class[_]) = BoolTypes contains klass



    private[swagger] def fromManifest[T](implicit mf: Manifest[T]): DataType = {
      fromScalaType(Reflector.scalaTypeOf[T])
    }
    private[swagger] def fromScalaType(st: ScalaType): DataType = {
      val klass = if (st.isOption && st.typeArgs.size > 0) st.typeArgs.head.erasure else st.erasure
      if (classOf[Unit].isAssignableFrom(klass) || classOf[Void].isAssignableFrom(klass)) this.Void
      else if (isString(klass)) this.String
      else if (classOf[Byte].isAssignableFrom(klass) || classOf[java.lang.Byte].isAssignableFrom(klass)) this.Byte
      else if (classOf[Long].isAssignableFrom(klass) || classOf[java.lang.Long].isAssignableFrom(klass)) this.Long
      else if (isInt(klass)) this.Int
      else if (classOf[Float].isAssignableFrom(klass) || classOf[java.lang.Float].isAssignableFrom(klass)) this.Float
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
      else if (classOf[scala.collection.Set[_]].isAssignableFrom(klass) || classOf[java.util.Set[_]].isAssignableFrom(klass)) {
        if (st.typeArgs.nonEmpty) GenSet(fromScalaType(st.typeArgs.head))
        else GenSet()
      }
      else if (classOf[collection.Seq[_]].isAssignableFrom(klass) || classOf[java.util.List[_]].isAssignableFrom(klass)) {
        if (st.typeArgs.nonEmpty) GenList(fromScalaType(st.typeArgs.head))
        else GenList()
      }
      else if (st.isArray || isCollection(klass)) {
        if (st.typeArgs.nonEmpty) GenArray(fromScalaType(st.typeArgs.head))
        else GenArray()
      }
      else {
        val stt = if (st.isOption) st.typeArgs.head else st
        new ValueDataType(stt.simpleName, qualifiedName = Option(stt.fullName))
      }
    }

    private[this] val IntTypes =
      Set[Class[_]](classOf[Int], classOf[java.lang.Integer], classOf[Short], classOf[java.lang.Short], classOf[BigInt], classOf[java.math.BigInteger])
    private[this] def isInt(klass: Class[_]) = IntTypes.contains(klass)

    private[this] val DecimalTypes =
      Set[Class[_]](classOf[Double], classOf[java.lang.Double], classOf[BigDecimal], classOf[java.math.BigDecimal])
    private[this] def isDecimal(klass: Class[_]) = DecimalTypes contains klass

    private[this] val DateTimeTypes =
      Set[Class[_]](classOf[Date], classOf[DateTime])
    private[this] def isDateTime(klass: Class[_]) = DateTimeTypes.exists(_.isAssignableFrom(klass))
    //
    //  private[this] def isMap(klass: Class[_]) =
    //    classOf[collection.Map[_, _]].isAssignableFrom(klass) ||
    //    classOf[java.util.Map[_, _]].isAssignableFrom(klass)

    private[this] def isCollection(klass: Class[_]) =
      classOf[collection.Traversable[_]].isAssignableFrom(klass) ||
        classOf[java.util.Collection[_]].isAssignableFrom(klass)

  }

  private def tagToManifest[T](tag: TypeTag[T]): Manifest[T] = {
    Manifest.classType(tag.mirror.runtimeClass(tag.tpe))
  }
}
