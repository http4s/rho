package org.http4s.rho.swagger

import java.sql.Timestamp
import java.util.Date

import cats.effect.IO
import cats.syntax.all._
import fs2.Stream
import org.http4s.rho.swagger.models.AbstractProperty
import org.specs2.execute.Result
import org.specs2.mutable.Specification
import shapeless.{:+:, CNil}

import scala.reflect.runtime.universe.{TypeTag, typeOf, typeTag}

package object model {
  case class Foo(a: Int, b: String)
  case class FooWithOption(a: Int, b: Option[String])
  type Bar = Foo
  case class FooDefault(a: Int = 0)
  case class FooGeneric[+A](a: A)
  type Foos = Seq[Foo]
  case class FooComposite(single: Foo, many: Seq[Foo])
  case class FooCompositeWithAlias(single: Bar, many: Seq[Bar], manyAlias: Foos)
  case class FooWithListOfLists(values: Seq[Seq[Int]])
  case class FooWithList(l: List[Int])
  case class FooWithMap(l: Map[String, Int])
  case class FooVal(foo: Foo) extends AnyVal
  case class BarWithFooVal(fooVal: FooVal)
  @DiscriminatorField("foobar")
  sealed trait Sealed {
    def foo: String
  }
  case class FooSealed(a: Int, foo: String, foo2: Foo) extends Sealed
  case class BarSealed(str: String, foo: String) extends Sealed

  sealed trait SealedEnum
  case object FooEnum extends SealedEnum
  case object BarEnum extends SealedEnum

  case class SealedEnumContainer(e: SealedEnum)

  sealed trait BTree
  case class Branch(left: BTree, right: BTree) extends BTree
  case class Leaf(value: String) extends BTree

  case class BarList(tail: Option[BarList])

  case class FooEither(stringOrFoo: Either[String, FooEither])

  sealed trait TopLevelSealedTrait
  case class Value1() extends TopLevelSealedTrait
  sealed trait LowerLevelSealedTrait extends TopLevelSealedTrait
  case class Value2() extends LowerLevelSealedTrait
  case class Value3() extends LowerLevelSealedTrait

  trait Outer[T]{
    case class Inner(t: T)
  }
  object OuterInt extends Outer[Int]

  type ShapelessIntOrString = Int :+: String :+: CNil
}

class TypeBuilderSpec extends Specification {
  import model._
  import models.{ArrayProperty, Model, RefProperty}

  def modelOf[T](implicit t: TypeTag[T]): Set[Model] =
    modelOfWithFormats(DefaultSwaggerFormats)

  def modelOfWithFormats[T](formats: SwaggerFormats)(implicit t: TypeTag[T]): Set[Model] =
    TypeBuilder.collectModels(t.tpe, Set.empty, formats, typeOf[IO[_]])

  "TypeBuilder" should {

    "Not Build a model for a primitive" in {

      val primitives = Set[TypeTag[_]](
        typeTag[String],               typeTag[Int],
        typeTag[Long],                 typeTag[Double],
        typeTag[Float],                typeTag[Byte],
        typeTag[BigInt],               typeTag[Boolean],
        typeTag[Short],                typeTag[java.lang.Integer],
        typeTag[java.lang.Long],       typeTag[java.lang.Double],
        typeTag[java.lang.Float],      typeTag[BigDecimal],
        typeTag[java.lang.Byte],       typeTag[java.lang.Boolean],
        typeTag[Number],               typeTag[java.lang.Short],
        typeTag[Date],                 typeTag[Timestamp],
        typeTag[Symbol],               typeTag[java.math.BigDecimal],
        typeTag[java.math.BigInteger]
      )

      val ms = primitives.foldLeft(Set.empty[Model]) { (s, t) =>
        TypeBuilder.collectModels(t.tpe, s, DefaultSwaggerFormats, typeOf[IO[_]])
      }

      ms.isEmpty must_== true
    }

    "Identify types" in {
      typeOf[IO[String]].isEffect(typeOf[IO[_]]) must_== true
      typeOf[String].isEffect(typeOf[IO[_]]) must_== false

      typeOf[Stream[IO, String]].isStream must_== true
      typeOf[String].isStream must_== false

      typeOf[Array[String]].isArray must_== true
      typeOf[String].isArray must_== false

      typeOf[Seq[String]].isCollection must_== true
      typeOf[java.util.Collection[String]].isCollection must_== true
      typeOf[String].isCollection must_== false

      typeOf[Either[Int,String]].isEither must_== true
      typeOf[String].isEither must_== false

      typeOf[Map[Int,String]].isMap must_== true
      typeOf[String].isMap must_== false

      typeOf[Nothing].isNothingOrNull must_== true
      typeOf[Null].isNothingOrNull must_== true
      typeOf[String].isNothingOrNull must_== false

      typeOf[Unit].isUnitOrVoid must_== true
      typeOf[Void].isUnitOrVoid must_== true
      typeOf[String].isUnitOrVoid must_== false

      typeOf[Option[String]].isOption must_== true
      typeOf[String].isOption must_== false

      Reflector.primitives.foreach(_.isPrimitive must_== true)
      typeOf[Some[String]].isPrimitive must_== false
    }

    "Build simple model" in {
      val m = modelOf[Foo].head
      m.description must_== "Foo".some
      m.properties.get("a").get.`type` must_== "integer"
      m.properties.get("a").get.required must_== true
    }

    "Build a model from alias" in {
      val m = modelOf[Bar].head
      m.description must_== "Foo".some
      m.properties.get("a").get.`type` must_== "integer"
      m.properties.get("a").get.required must_== true
    }

    "Build a model with a default" in {
      val ms = modelOf[FooDefault]
      ms.size must_== 1

      val m = ms.head

      m.description must_== "FooDefault".some
      m.properties.size must_== 1
      m.properties.get("a").get.`type` must_== "integer"
      m.properties.get("a").get.required must_== false
    }

    "Build a model with a generic" in {
      val ms = modelOf[FooGeneric[Int]]
      ms.size must_== 1
      val m = ms.head
      m.description must_== "FooGeneric«Int»".some

      m.properties.size must_== 1
      m.properties.get("a").get.`type` must_== "integer"
    }

    "Build a model with a generic of type Nothing" in {
      val ms = modelOf[FooGeneric[Nothing]]
      ms.size must_== 2
      ms.find(_.id2 == "FooGeneric«Nothing»") must beSome.which { m =>
        m.properties.size must_== 1
        m.properties("a").`type` must_== "Nothing"
      }

      ms.find(_.id2 == "Nothing") must beSome
    }

    "Build a model with a generic of type Null" in {
      val ms = modelOf[FooGeneric[Null]]
      ms.size must_== 2
      ms.find(_.id2 == "FooGeneric«Null»") must beSome.which { m =>
        m.properties.size must_== 1
        m.properties("a").`type` must_== "Null"
      }

      ms.find(_.id2 == "Null") must beSome
    }

    "Build a model with a generic of type Unit" in {
      val ms = modelOf[FooGeneric[Unit]]
      ms.size must_== 2
      ms.find(_.id2 == "FooGeneric«Unit»") must beSome.which { m =>
        m.properties.size must_== 1
        m.properties("a").`type` must_== "Unit"
      }

      ms.find(_.id2 == "Unit") must beSome
    }

    "Build a model with a generic of type Void" in {
      val ms = modelOf[FooGeneric[Void]]
      ms.size must_== 2
      ms.find(_.id2 == "FooGeneric«Void»") must beSome.which { m =>
        m.properties.size must_== 1
        m.properties("a").`type` must_== "Void"
      }

      ms.find(_.id2 == "Void") must beSome
    }

    "Build a composite model" in {
      val ms = modelOf[FooComposite]
      ms.size must_== 2
      val m1 = ms.head
      m1.description must_== "FooComposite".some
      m1.properties.size must_== 2
      m1.properties.get("single").get.`type` must_== "ref"

      m1.properties.get("many") must beLike {
        case Some(array: ArrayProperty) =>

          array.items must beLike {
            case ref: RefProperty =>
              ref.ref must_== "Foo"
          }
      }

      val m2 = ms.tail.head
      m2.description must_== "Foo".some
    }

    "Build a composite model with alias" in {
      val ms = modelOf[FooCompositeWithAlias]
      ms.size must_== 2
      val m1 = ms.head
      m1.description must_== "FooCompositeWithAlias".some
      m1.properties.size must_== 3
      m1.properties.get("single").get.`type` must_== "string"
      m1.properties.get("many").get.`type` must_== "array"
      m1.properties.get("manyAlias").get.`type` must_== "array"

      val m2 = ms.tail.head
      m2.description must_== "Foo".some
    }

    "Build a model with a non-basic generic" in {
      val ms = modelOf[FooGeneric[Foo]]
      ms.size must_== 2
      ms ++ modelOf[Foo] must_== ms
    }

    "Build a model from an Option" in {
      val ms = modelOf[FooWithOption]
      ms.size must_== 1
      val omodel = ms.head
      val optProp = omodel.properties("b")
      optProp.required must_== false
    }

    "Build a model from an Option with overridden formats" in {
      val formats = DefaultSwaggerFormats.withFieldSerializers({
        case x if x =:= typeOf[String] => AbstractProperty(`type`="a_string")
      })

      val ms = modelOfWithFormats[FooWithOption](formats)
      ms.size must_== 1
      val omodel = ms.head
      val optProp = omodel.properties("b")
      optProp.required must_== false
      optProp.`type` must_== "a_string"
    }

    "Get types from a collection" in {
      modelOf[Seq[Foo]] must_== modelOf[Foo]
      modelOf[Map[String, Foo]] must_== modelOf[Foo]
    }

    "Get types from a fs2.Stream" in {
      modelOf[Stream[IO, Foo]] must_== modelOf[Foo]
    }

    "Get types from an IO Effect" in {
      modelOf[IO[Foo]] must_== modelOf[Foo]
    }

    "Get types from a SwaggerFileResponse" in {
      modelOf[SwaggerFileResponse[Foo]] must_== Set.empty
    }

    "Build model that contains a List" in {
      val ms = modelOf[FooWithList]
      ms.size must_== 1
      val m = ms.head
      m.description must_== "FooWithList".some

      m.properties.head._2 must beLike {
        case items: ArrayProperty =>
          items.`type` must_== "array"
          items.required must_== true

          items.items.required must_== false
          items.items.`type` must_== "integer"
          items.items.format must_== Some("int32")
      }
    }

    "Build model that contains a List of Lists" in {
      val ms = modelOf[FooWithListOfLists]
      ms.size must_== 1
      val m = ms.head
      m.description must_== "FooWithListOfLists".some

      m.properties.head._2 must beLike {
        case items: ArrayProperty =>
          items.`type` must_== "array"
          items.required must_== true

          items.items must beLike {
            case items: ArrayProperty =>
              items.`type` must_== "array"

              items.items.required must_== false
              items.items.`type` must_== "integer"
              items.items.format must_== Some("int32")
          }
      }
    }

    "Treat AnyVals transparently" in {
      modelOf[FooVal] must_== modelOf[Foo]
    }

    "Build a model with the underlying type for AnyVals" in {
      val ms = modelOf[BarWithFooVal]

      ms.size must_== 2
      val Some(m) = ms.find(_.id2 == "BarWithFooVal")

      m.properties.size must_== 1
      m.properties.head must beLike {
        case (name, prop: RefProperty) =>
          name must_== "fooVal"
          prop.ref  must_== "Foo"
      }
    }

    "Build a model for sealed traits" in {
      val ms = modelOf[Sealed]
      ms.foreach(_.toJModel) // Testing that there are no exceptions
      ms.size must_== 4
      ms.find(_.id2 == "Foo").get must haveClass[models.ModelImpl]  // Foo should not become a ComposedModel
      val Some(seald: models.ModelImpl) = ms.find(_.id2 == "Sealed")
      seald.discriminator must_== Some("foobar")
      val Some(sealedFoo: models.ComposedModel) = ms.find(_.id2 == "FooSealed")
      val Some(fooRef) = sealedFoo.allOf.collectFirst {case ref: models.RefModel => ref}
      fooRef.ref must_== "Sealed"
    }

    "Not modify unrelated types when building model for sealed trait" in {
      val unrelatedModel = modelOf[FooDefault]
      val model = TypeBuilder.collectModels(typeOf[Sealed], unrelatedModel, DefaultSwaggerFormats, typeOf[IO[_]])
      model must_== modelOf[Sealed]
    }

    "Build a model for two-level sealed trait hierarchy" in {
      val ms = modelOf[TopLevelSealedTrait]
      ms.size must_== 5

      val Some(value1: models.ComposedModel) = ms.find(_.id2 == "Value1")
      val Some(value1Parent: models.RefModel) = value1.parent
      value1Parent.id2 must_== "TopLevelSealedTrait"

      val Some(loweLevel: models.ComposedModel) = ms.find(_.id2 == "LowerLevelSealedTrait")
      val Some(loweLevelParent: models.RefModel) = loweLevel.parent
      loweLevelParent.id2 must_== "TopLevelSealedTrait"

      val Some(value2: models.ComposedModel) = ms.find(_.id2 == "Value2")
      val Some(value2Parent: models.RefModel) = value2.parent
      value2Parent.id2 must_== "LowerLevelSealedTrait"
    }

    "Build identical model for sealed trait regardless of the entry point" in {
      modelOf[TopLevelSealedTrait] must_== modelOf[LowerLevelSealedTrait]
      modelOf[LowerLevelSealedTrait] must_== modelOf[Value1]
      modelOf[Value1] must_== modelOf[Value2]
    }

    "Build a model for a case class containing a sealed enum" in {
      val ms = modelOf[SealedEnumContainer]
      ms.size must_== 1
    }

    "Build a model for a type recursive through sealed trait" in {
      modelOf[BTree] must not(throwA[StackOverflowError])
      modelOf[BTree].size must_== 3
    }

    "Build a model for a type recursive through Option" in {
      modelOf[BarList] must not(throwA[StackOverflowError])
      modelOf[BarList].size must_== 1
    }

    "Build a model for a type recursive through Either" in {
      modelOf[FooEither] must not(throwA[StackOverflowError])
      modelOf[FooEither].size must_== 1
    }

    "Build a model for a class using type parameters of an outer class" in {
      val ms = modelOf[OuterInt.Inner]
      ms.size must_== 1
      val m = ms.head
      val t = m.properties.get("t")
      t should not be empty
      t.get.`type` must_== "integer"
      t.get.format must beSome("int32")
    }

    "Build a model for shapless coproduct (:+:)" in {
      val ms = modelOf[ShapelessIntOrString]
      ms.size must_!== 0
      // It won't be a fully correct model.
    }
  }

  "DataType" should {
    import org.http4s.rho.swagger.TypeBuilder.DataType

    "Get the correct name for primitives" in {
      DataType(typeTag[Int]).name must_== "integer"
      DataType(typeTag[String]).name must_== "string"
    }

    "Get the type for a container" in {
      DataType(typeTag[Seq[Int]]).name must_== "List"
    }

    "Get the DataType of the underlying type for an AnyVal" in {
      val datatypes = List[(TypeTag[_], TypeTag[_])](
        (typeTag[StringAnyVal], typeTag[String]), (typeTag[IntAnyVal], typeTag[Int]),
        (typeTag[DoubleAnyVal], typeTag[Double]), (typeTag[EntityAnyVal], typeTag[Entity])
      )

      Result.foreach(datatypes) { case (anyValType, underlyingType) =>
        DataType(anyValType) === DataType(underlyingType)
      }
    }

    "Get the DataType for sealed enums" in {
      DataType(typeTag[SealedEnum]) must_== DataType.EnumDataType(Set("BarEnum", "FooEnum"))
    }
  }
}

case class StringAnyVal(value: String) extends AnyVal
case class IntAnyVal(value: Int) extends AnyVal
case class DoubleAnyVal(value: Double) extends AnyVal
case class EntityAnyVal(value: Entity) extends AnyVal

case class Entity(name: String)
