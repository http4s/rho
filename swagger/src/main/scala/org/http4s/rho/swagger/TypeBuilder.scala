package org.http4s.rho.swagger

import java.util.Date
import java.time.Instant

import org.log4s.getLogger

import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

import cats.syntax.all._

case class DiscriminatorField(field: String) extends scala.annotation.StaticAnnotation

object TypeBuilder {
  import models._

  private[this] val logger = getLogger

  def collectModels(t: Type, alreadyKnown: Set[Model], sfs: SwaggerFormats, et: Type): Set[Model] =
    try collectModels(t.dealias, alreadyKnown, Set.empty, sfs, et)
    catch { case NonFatal(e) => Set.empty }

  private def collectModels(t: Type, alreadyKnown: Set[Model], known: Set[Type], sfs: SwaggerFormats, et: Type): Set[Model] = {

    def go(t: Type, alreadyKnown: Set[Model], known: Set[Type]): Set[Model] =
      t.dealias match {

        case tpe if sfs.customSerializers.isDefinedAt(tpe) =>
          sfs.customSerializers(tpe)

        case tpe if tpe.isNothingOrNull || tpe.isUnitOrVoid =>
          alreadyKnown ++ modelToSwagger(tpe, sfs)

        case tpe if tpe.isEither || tpe.isMap =>
          go(tpe.typeArgs.head, alreadyKnown, tpe.typeArgs.toSet) ++
            go(tpe.typeArgs.last, alreadyKnown, tpe.typeArgs.toSet)

        case tpe if (tpe.isCollection || tpe.isOption) && tpe.typeArgs.nonEmpty =>
          val ntpe = tpe.typeArgs.head
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isStream =>
          val ntpe = tpe.typeArgs.apply(1)
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isEffect(et) =>
          val ntpe = tpe.typeArgs.head
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isSwaggerFile =>
          Set.empty

        case tpe if alreadyKnown.map(_.id).contains(tpe.fullName) || tpe.isPrimitive =>
          Set.empty

        case tpe if tpe <:< typeOf[AnyVal] =>
          Set.empty

        case ExistentialType(_, _) =>
          Set.empty

        case tpe@TypeRef(_, sym: Symbol, tpeArgs: List[Type]) if isCaseClass(sym) =>
          val ctor = sym.asClass.primaryConstructor.asMethod
          val models = alreadyKnown ++ modelToSwagger(tpe, sfs)
          val generics = tpe.typeArgs.foldLeft(List[Model]()) { (acc, t) =>
            acc ++ go(t, alreadyKnown, tpe.typeArgs.toSet)
          }
          val children = ctor.paramLists.flatten.flatMap { paramsym =>
            val paramType =
              if (sym.isClass)
                paramsym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
              else
                sym.typeSignature
            go(paramType, alreadyKnown, known + tpe)
          }

          models ++ generics ++ children

        case tpe@TypeRef(_, sym: Symbol, tpeArgs: List[Type]) if isObjectEnum(sym) =>
          Set.empty

        case tpe@TypeRef(_, sym: Symbol, tpeArgs: List[Type]) if isSumType(sym) =>
          // TODO promote methods on sealed trait from children to model
          modelToSwagger(tpe, sfs).map(addDiscriminator(sym)).toSet.flatMap { (model: Model) =>
            val refmodel = RefModel(model.id, model.id2, model.id2)
            val children =
              sym.asClass.knownDirectSubclasses.flatMap { sub =>
                go(sub.asType.toType, alreadyKnown, known + tpe).map(composedModel(refmodel))
              }
            alreadyKnown ++ Set(model) ++ children
          }

        case e =>
          Set.empty
      }

    go(t, alreadyKnown, known)
  }

  private def addDiscriminator(sym: Symbol)(model: ModelImpl): ModelImpl = {
    val typeVar = sym.annotations
      .withFilter(_.tree.tpe <:< typeOf[DiscriminatorField])
      .flatMap(_.tree.children.tail.collect { case Literal(Constant(field: String)) => field } )
      .headOption.getOrElse("type")
    val subclasses = sym.asClass.knownDirectSubclasses.map(_.asType.toType.simpleName)

    model
      .copy(
        discriminator = Some(typeVar),
        `type` = Some("object"),
        properties =
        model.properties + (typeVar -> StringProperty(required = true, enums = subclasses))
      )
  }

  private def composedModel(parent: RefModel)(subtype: Model): Model = {
    ComposedModel(
      id = subtype.id,
      id2 = subtype.id2,
      description = subtype.description,
      allOf = List(parent, subtype),
      parent = parent.some
    )
  }

  private[this] val defaultExcluded =
    Set(typeOf[Nothing], typeOf[Null])

  private[this] def isCaseClass(sym: Symbol): Boolean =
    sym.isClass && sym.asClass.isCaseClass && sym.asClass.primaryConstructor.isMethod

  private[this] def isSumType(sym: Symbol): Boolean =
    sym.isClass && sym.asClass.isSealed && sym.asClass.knownDirectSubclasses.forall { symbol =>
      !symbol.isModuleClass && symbol.asClass.isCaseClass
    }

  private[this] def isObjectEnum(sym: Symbol): Boolean =
    sym.asClass.isSealed && sym.asClass.knownDirectSubclasses.forall { symbol =>
      symbol.isModuleClass && symbol.asClass.isCaseClass
    }

  private[this] def isExcluded(t: Type, excludes: Seq[Type] = Nil) =
    (defaultExcluded ++ excludes).exists(_ =:= t)

  private def modelToSwagger(tpe: Type, sfs: SwaggerFormats): Option[ModelImpl] =
    try {
      val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
      val props: Map[String, Property] =
        tpe
          .member(termNames.CONSTRUCTOR)
          .typeSignature
          .paramLists
          .flatten
          .map(paramSymToProp(sym, tpeArgs, sfs))
          .toMap

      ModelImpl(
        id          = tpe.fullName,
        id2         = tpe.simpleName,
        description = tpe.simpleName.some,
        `type`      = "object".some,
        properties  = props).some
    } catch {
      case NonFatal(t) =>
        logger.info(t)(s"Failed to build model for type $tpe")
        None
    }

  private def paramSymToProp
  (sym: Symbol, tpeArgs: List[Type], sfs: SwaggerFormats)(pSym: Symbol): (String, Property) = {
    val pType = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
    val required = !(pSym.asTerm.isParamWithDefault || pType.isOption)
    val prop = typeToProperty(pType, sfs)
    (pSym.name.decodedName.toString, prop.withRequired(required))
  }

  // Turn a `Type` into the appropriate `Property` representation
  private def typeToProperty(tpe: Type, sfs: SwaggerFormats): Property = {
    sfs.customFieldSerializers.applyOrElse(tpe, { _: Type =>
      val TypeRef(_, ptSym: Symbol, _) = tpe

      if (tpe.isMap && !tpe.isNothingOrNull) {
        val pType = tpe.dealias.typeArgs.last
        val itemProperty = typeToProperty(pType, sfs).withRequired(false)
        MapProperty(additionalProperties = itemProperty)
      }
      else if (tpe.isCollection && !tpe.isNothingOrNull) {
        val pType = tpe.dealias.typeArgs.head
        val itemProperty = typeToProperty(pType, sfs).withRequired(false)
        ArrayProperty(items = itemProperty)
      }
      else if (tpe.isOption && !tpe.isNothingOrNull)
        typeToProperty(tpe.typeArgs.head, sfs).withRequired(false)
      else if ((isCaseClass(ptSym) || isSumType(ptSym)) && !(tpe <:< typeOf[AnyVal]))
        RefProperty(tpe.simpleName)
      else
        DataType.fromType(tpe) match {
          case DataType.ValueDataType(name, format, qName) =>
            AbstractProperty(`type` = name, description = qName, format = format)
          case DataType.ComplexDataType(name, qName) =>
            AbstractProperty(`type` = name, description = qName)
          case DataType.ContainerDataType(name, tpe, uniqueItems) =>
            AbstractProperty(`type` = name)
          case DataType.EnumDataType(enums) =>
            StringProperty(enums = enums)
        }
    })
  }

  sealed trait DataType {
    def name: String
  }

  object DataType {

    case class ValueDataType(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) extends DataType
    case class ContainerDataType(name: String, typeArg: Option[DataType] = None, uniqueItems: Boolean = false) extends DataType
    case class ComplexDataType(name: String, qualifiedName: Option[String] = None) extends DataType
    case class EnumDataType(enums: Set[String]) extends DataType { val name = "string" }

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

    def apply(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) =
      new ValueDataType(name, format, qualifiedName)

    def apply(tag: TypeTag[_]): DataType = apply(tag.tpe)
    def apply(tag: Type): DataType = fromType(tag.dealias)

    private[this] val StringTypes = Set[Type](typeOf[String], typeOf[java.lang.String])
    private[this] def isString(t: Type) = StringTypes.exists(t =:= _)
    private[this] val BoolTypes = Set[Type](typeOf[Boolean], typeOf[java.lang.Boolean])
    private[this] def isBool(t: Type) = BoolTypes.exists(t =:= _)

    private[swagger] def fromType(t: Type): DataType = {
      val klass = if (t.isOption && t.typeArgs.nonEmpty) t.typeArgs.head else t

      if (klass.isNothingOrNull || klass.isUnitOrVoid) ComplexDataType(klass.simpleName, qualifiedName = Option(klass.fullName))
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
      } else if (t.isStream) {
        if (t.typeArgs.nonEmpty) GenArray(fromType(t.typeArgs(1)))
        else GenArray()
      } else if (klass <:< typeOf[AnyVal]) {
        fromType(klass.members.filter(_.isConstructor).flatMap(_.asMethod.paramLists.flatten).head.typeSignature)
      } else if (isObjectEnum(klass.typeSymbol)) {
        EnumDataType(klass.typeSymbol.asClass.knownDirectSubclasses.map(_.name.toString))
      } else {
        val stt = if (t.isOption) t.typeArgs.head else t
        ComplexDataType(stt.simpleName, qualifiedName = Option(stt.fullName))
      }
    }

    private[this] val IntTypes =
      Set[Type](
        typeOf[Int], typeOf[java.lang.Integer], typeOf[Short],
        typeOf[java.lang.Short], typeOf[BigInt], typeOf[java.math.BigInteger])

    private[this] def isInt(t: Type): Boolean = IntTypes.exists(t =:= _)

    private[this] val DecimalTypes =
      Set[Type](
        typeOf[Double], typeOf[java.lang.Double],
        typeOf[BigDecimal], typeOf[java.math.BigDecimal])

    private[this] def isDecimal(t: Type): Boolean =
      DecimalTypes.exists(t =:= _)

    private[this] val DateTimeTypes =
      Set[Type](typeOf[Date], typeOf[Instant])

    private[this] def isDateTime(t: Type): Boolean =
      DateTimeTypes.exists(t <:< _)

    private[this] def isCollection(t: Type): Boolean =
      t <:< typeOf[collection.Traversable[_]] || t <:< typeOf[java.util.Collection[_]]
  }
}
