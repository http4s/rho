package org.http4s.rho.swagger

import java.sql.Timestamp
import java.util.Date
import cats.effect.IO
import cats.syntax.all._
import fs2.Stream
import munit.FunSuite
import org.http4s.rho.swagger.TypeBuilder.DataType
import org.http4s.rho.swagger.models.{AbstractProperty, Model, StringProperty}
import shapeless.{:+:, CNil}

import scala.reflect.runtime.universe.{TypeTag, typeOf, typeTag}
import scala.collection.immutable.Seq

package object model {
  case class Foo(a: Int, b: String)
  case class FooWithOption(a: Int, b: Option[String])
  type Bar = Foo
  case class FooDefault(a: Int = 0)
  case class FooGeneric[+A](a: A)
  case class FooDoubleGeneric[+A, +B](a: A, b: B)
  type Foos = Seq[Foo]
  case class FooComposite(single: Foo, many: Seq[Foo])
  case class FooCompositeWithAlias(single: Bar, many: Seq[Bar], manyAlias: Foos)
  case class FooWithListOfLists(values: Seq[Seq[Int]])
  case class FooWithList(l: List[Int])
  case class FooWithMap(l: Map[String, Int])
  case class FooVal(foo: Foo) extends AnyVal
  case class BarWithFooVal(fooVal: FooVal)
  case class AnyValClass(anyVal: AnyVal)
  type AnyValType = AnyVal
  case class ClassWithAnyValType(anyVal: AnyValType)
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

  sealed trait MixedSealed
  case class CaseClass() extends MixedSealed
  case object CaseObject extends MixedSealed

  trait Outer[T] {
    case class Inner(t: T)
  }
  object OuterInt extends Outer[Int]

  type ShapelessIntOrString = Int :+: String :+: CNil

  def modelOf[T](implicit t: TypeTag[T], st: ShowType): Set[Model] =
    modelOfWithFormats(DefaultSwaggerFormats)

  def modelOfWithFormats[T](
                             formats: SwaggerFormats)(implicit t: TypeTag[T], st: ShowType): Set[Model] =
    TypeBuilder.collectModels(t.tpe, Set.empty, formats, typeOf[IO[_]])
}

class TypeBuilderSuite extends FunSuite {
  import model._
  import models.{ArrayProperty, Model, RefProperty}

  implicit val showType: ShowType = DefaultShowType

  test("A TypeBuilder should not Build a model for a primitive") {
    val primitives = Set[TypeTag[_]](
      typeTag[String],
      typeTag[Int],
      typeTag[Long],
      typeTag[Double],
      typeTag[Float],
      typeTag[Byte],
      typeTag[BigInt],
      typeTag[Boolean],
      typeTag[Short],
      typeTag[java.lang.Integer],
      typeTag[java.lang.Long],
      typeTag[java.lang.Double],
      typeTag[java.lang.Float],
      typeTag[BigDecimal],
      typeTag[java.lang.Byte],
      typeTag[java.lang.Boolean],
      typeTag[Number],
      typeTag[java.lang.Short],
      typeTag[Date],
      typeTag[Timestamp],
      typeTag[Symbol],
      typeTag[java.math.BigDecimal],
      typeTag[java.math.BigInteger]
    )

    val ms = primitives.foldLeft(Set.empty[Model]) { (s, t) =>
      TypeBuilder.collectModels(t.tpe, s, DefaultSwaggerFormats, typeOf[IO[_]])
    }

    assertEquals(ms, Set.empty[Model])
  }

  test("A TypeBuilder should identify types") {
    assert(typeOf[IO[String]].isEffect(typeOf[IO[_]]))
    assertEquals(typeOf[String].isEffect(typeOf[IO[_]]), false)

    assert(typeOf[Stream[IO, String]].isStream)
    assertEquals(typeOf[String].isStream, false)

    assert(typeOf[Array[String]].isArray)
    assertEquals(typeOf[String].isArray, false)

    assert(typeOf[Seq[String]].isCollection)
    assert(typeOf[java.util.Collection[String]].isCollection)
    assertEquals(typeOf[String].isCollection, false)

    assert(typeOf[Either[Int, String]].isEither)
    assertEquals(typeOf[String].isEither, false)

    assert(typeOf[Map[Int, String]].isMap, true)
    assertEquals(typeOf[String].isMap, false)

    assert(typeOf[Nothing].isNothingOrNull)
    assert(typeOf[Null].isNothingOrNull)
    assertEquals(typeOf[String].isNothingOrNull, false)

    assert(typeOf[Unit].isUnitOrVoid, true)
    assert(typeOf[Void].isUnitOrVoid, true)
    assertEquals(typeOf[String].isUnitOrVoid, false)

    assert(typeOf[Option[String]].isOption, true)
    assertEquals(typeOf[String].isOption, false)

    assert(Reflector.primitives.forall(_.isPrimitive == true))
    assertEquals(typeOf[Some[String]].isPrimitive, false)
  }

  test("A TypeBuilder should build simple model") {
    val m = modelOf[Foo].head
    assertEquals(m.description, "Foo".some)
    assertEquals(m.properties.get("a").get.`type`, "integer")
    assert(m.properties.get("a").get.required)
  }

  test("A TypeBuilder should build a model from alias") {
    val m = modelOf[Bar].head
    assertEquals(m.description, "Foo".some)
    assertEquals(m.properties.get("a").get.`type`, "integer")
    assert(m.properties.get("a").get.required)
  }

  test("A TypeBuilder should build a model with a default") {
    val ms = modelOf[FooDefault]
    assertEquals(ms.size, 1)

    val m = ms.head

    assertEquals(m.description, "FooDefault".some)
    assertEquals(m.properties.size, 1)
    assertEquals(m.properties.get("a").get.`type`, "integer")
    assertEquals(m.properties.get("a").get.required, false)
  }

  test("A TypeBuilder should build a model with a generic") {
    val ms = modelOf[FooGeneric[Int]]
    assertEquals(ms.size, 1)
    val m = ms.head
    assertEquals(m.description, "FooGeneric«Int»".some)

    assertEquals(m.properties.size, 1)
    assertEquals(m.properties.get("a").get.`type`, "integer")
  }

  test("A TypeBuilder should build a model with a generic of type Nothing") {
    val ms = modelOf[FooGeneric[Nothing]]
    assertEquals(ms.size, 2)
    ms.find(_.id2 == "FooGeneric«Nothing»")
      .fold(fail("A TypeBuilder should build a model with a generic of type Nothing")) { m =>
        assertEquals(m.properties.size, 1)
        assertEquals(m.properties("a").asInstanceOf[RefProperty].ref, "Nothing")
      }

    assert(ms.exists(_.id2 == "Nothing"))
  }

  test("A TypeBuilder should build a model with a generic of type Null") {
    val ms = modelOf[FooGeneric[Null]]
    assertEquals(ms.size, 2)
    ms.find(_.id2 == "FooGeneric«Null»")
      .fold(fail("A TypeBuilder should build a model with a generic of type Null")) { m =>
        assertEquals(m.properties.size, 1)
        assertEquals(m.properties("a").asInstanceOf[RefProperty].ref, "Null")
      }

    assert(ms.exists(_.id2 == "Null"))
  }

  test("A TypeBuilder should build a model with a generic of type Unit") {
    val ms = modelOf[FooGeneric[Unit]]
    assertEquals(ms.size, 2)
    ms.find(_.id2 == "FooGeneric«Unit»")
      .fold(fail("A TypeBuilder should build a model with a generic of type Unit")) { m =>
        assertEquals(m.properties.size, 1)
        assertEquals(m.properties("a").asInstanceOf[RefProperty].ref, "Unit")
      }

    assert(ms.exists(_.id2 == "Unit"))
  }

  test("A TypeBuilder should build a model with a generic of type Void") {
    val ms = modelOf[FooGeneric[Void]]
    assertEquals(ms.size, 2)
    ms.find(_.id2 == "FooGeneric«Void»")
      .fold(fail("A TypeBuilder should build a model with a generic of type Void")) { m =>
        assertEquals(m.properties.size, 1)
        assertEquals(m.properties("a").asInstanceOf[RefProperty].ref, "Void")
      }

    assert(ms.exists(_.id2 == "Void"))
  }

  test("A TypeBuilder should build a composite model") {
    val ms = modelOf[FooComposite]
    assertEquals(ms.size, 2)
    val m1 = ms.head
    assertEquals(m1.description, "FooComposite".some)
    assertEquals(m1.properties.size, 2)
    assertEquals(m1.properties.get("single").get.`type`, "ref")

    m1.properties.get("many") match {
      case Some(array: ArrayProperty) =>
        array.items match {
          case ref: RefProperty =>
            assertEquals(ref.ref, "Foo")

          case _ =>
            fail("A TypeBuilder should build a composite model")
        }

      case _ =>
        fail("A TypeBuilder should build a composite model")
    }

    val m2 = ms.tail.head
    assertEquals(m2.description, "Foo".some)
  }

  test("A TypeBuilder should build a composite model with alias") {
    val ms = modelOf[FooCompositeWithAlias]
    assertEquals(ms.size, 2)
    val m1 = ms.head
    assertEquals(m1.description, "FooCompositeWithAlias".some)
    assertEquals(m1.properties.size, 3)
    assertEquals(m1.properties.get("single").get.`type`, "string")
    assertEquals(m1.properties.get("many").get.`type`, "array")
    assertEquals(m1.properties.get("manyAlias").get.`type`, "array")

    val m2 = ms.tail.head
    assertEquals(m2.description, "Foo".some)
  }

  test("A TypeBuilder should build a model with a non-basic generic") {
    val ms = modelOf[FooGeneric[Foo]]
    assertEquals(ms.size, 2)
    assertEquals(ms ++ modelOf[Foo], ms)
  }

  test("A TypeBuilder should build a model from an Option") {
    val ms = modelOf[FooWithOption]
    assertEquals(ms.size, 1)
    val omodel = ms.head
    val optProp = omodel.properties("b")
    assertEquals(optProp.required, false)
  }

  test("A TypeBuilder should build a model from an Option with overridden formats") {
    val formats = DefaultSwaggerFormats.withFieldSerializers({
      case x if x =:= typeOf[String] => AbstractProperty(`type` = "a_string")
    })

    val ms = modelOfWithFormats[FooWithOption](formats)
    assertEquals(ms.size, 1)
    val omodel = ms.head
    val optProp = omodel.properties("b")
    assertEquals(optProp.required, false)
    assertEquals(optProp.`type`, "a_string")
  }

  test("A TypeBuilder should get types from a collection") {
    assertEquals(modelOf[Seq[Foo]], modelOf[Foo])
    assertEquals(modelOf[Map[String, Foo]], modelOf[Foo])
  }

  test("A TypeBuilder should get types from a fs2.Stream") {
    assertEquals(modelOf[Stream[IO, Foo]], modelOf[Foo])
  }

  test("A TypeBuilder should get types from an IO Effect") {
    assertEquals(modelOf[IO[Foo]], modelOf[Foo])
  }

  test("A TypeBuilder should get types from a SwaggerFileResponse") {
    assertEquals(modelOf[SwaggerFileResponse[Foo]], Set.empty[Model])
  }

  test("A TypeBuilder should build model that contains a List") {
    val ms = modelOf[FooWithList]
    assertEquals(ms.size, 1)
    val m = ms.head
    assertEquals(m.description, "FooWithList".some)

    m.properties.head._2 match {
      case items: ArrayProperty =>
        assertEquals(items.`type`, "array")
        assert(items.required, true)
        assertEquals(items.items.required, false)
        assertEquals(items.items.`type`, "integer")
        assertEquals(items.items.format, Some("int32"))

      case _ =>
        fail("A TypeBuilder should build model that contains a List")
    }
  }

  test("A TypeBuilder should build model that contains a List of Lists") {
    val ms = modelOf[FooWithListOfLists]
    assertEquals(ms.size, 1)
    val m = ms.head
    assertEquals(m.description, "FooWithListOfLists".some)

    m.properties.head._2 match {
      case items: ArrayProperty =>
        assertEquals(items.`type`, "array")
        assert(items.required)

        items.items match {
          case items: ArrayProperty =>
            assertEquals(items.`type`, "array")
            assertEquals(items.items.required, false)
            assertEquals(items.items.`type`, "integer")
            assertEquals(items.items.format, Some("int32"))

          case _ =>
            fail("A TypeBuilder should build model that contains a List of Lists")
        }

      case _ =>
        fail("A TypeBuilder should build model that contains a List of Lists")
    }
  }

  test("A TypeBuilder should treat AnyVals transparently") {
    assertEquals(modelOf[FooVal], modelOf[Foo])
  }

  test("A TypeBuilder should build a model with the underlying type for AnyVals") {
    val ms = modelOf[BarWithFooVal]

    assertEquals(ms.size, 2)
    val Some(m) = ms.find(_.id2 == "BarWithFooVal")

    assertEquals(m.properties.size, 1)
    m.properties.head match {
      case (name, prop: RefProperty) =>
        assertEquals(name, "fooVal")
        assertEquals(prop.ref, "Foo")

      case _ =>
        fail("A TypeBuilder should build a model with the underlying type for AnyVals")
    }
  }

  test("A TypeBuilder should build a model for sealed traits") {
    val ms = modelOf[Sealed]
    ms.foreach(_.toJModel) // Testing that there are no exceptions
    assertEquals(ms.size, 4)

    ms.find(_.id2 == "Foo").get match {
      case _: models.ModelImpl => () // Foo should not become a ComposedModel
      case _ => fail("Foo should not become a ComposedModel")
    }

    val Some(seald: models.ModelImpl) = ms.find(_.id2 == "Sealed")
    assertEquals(seald.discriminator, Some("foobar"))
    val Some(sealedFoo: models.ComposedModel) = ms.find(_.id2 == "FooSealed")
    val Some(fooRef) = sealedFoo.allOf.collectFirst { case ref: models.RefModel => ref }
    assertEquals(fooRef.ref, "Sealed")
  }

  test("A TypeBuilder should not modify unrelated types when building model for sealed trait") {
    val unrelatedModel = modelOf[FooDefault]
    val model = TypeBuilder.collectModels(
      typeOf[Sealed],
      unrelatedModel,
      DefaultSwaggerFormats,
      typeOf[IO[_]]
    )
    assertEquals(model, modelOf[Sealed])
  }

  test("A TypeBuilder should fall back to the class name for a class containing an AnyVal") {
    val m = modelOf[AnyValClass].head
    assertEquals(m.description, "AnyValClass".some)
  }

  test("A TypeBuilder should fall back to the class name for a custom type containing an AnyVal") {
    val m = modelOf[ClassWithAnyValType].head
    assertEquals(m.description, "ClassWithAnyValType".some)
  }

  test("A TypeBuilder should build a model for two-level sealed trait hierarchy") {
    val ms = modelOf[TopLevelSealedTrait]
    assertEquals(ms.size, 5)

    val Some(value1: models.ComposedModel) = ms.find(_.id2 == "Value1")
    val Some(value1Parent: models.RefModel) = value1.parent
    assertEquals(value1Parent.id2, "TopLevelSealedTrait")

    val Some(loweLevel: models.ComposedModel) = ms.find(_.id2 == "LowerLevelSealedTrait")
    val Some(loweLevelParent: models.RefModel) = loweLevel.parent
    assertEquals(loweLevelParent.id2, "TopLevelSealedTrait")

    val Some(value2: models.ComposedModel) = ms.find(_.id2 == "Value2")
    val Some(value2Parent: models.RefModel) = value2.parent
    assertEquals(value2Parent.id2, "LowerLevelSealedTrait")
  }

  test(
    "A TypeBuilder should build identical model for sealed trait regardless of the entry point"
  ) {
    assertEquals(modelOf[TopLevelSealedTrait], modelOf[LowerLevelSealedTrait])
    assertEquals(modelOf[LowerLevelSealedTrait], modelOf[Value1])
    assertEquals(modelOf[Value1], modelOf[Value2])
  }

  test(
    "A TypeBuilder should build identical model for case objects and empty case classes belonging to a sealed trait"
  ) {
    val ms = modelOf[MixedSealed]
    assertEquals(ms.size, 3)

    val Some(caseClass: models.ComposedModel) = ms.find(_.id2 == "CaseClass")
    val Some(caseClassParent: models.RefModel) = caseClass.parent
    assertEquals(caseClassParent.id2, "MixedSealed")
    val Some(caseClassInnerModel) = caseClass.allOf.find(_.id2 == "CaseClass")

    val Some(caseObject: models.ComposedModel) = ms.find(_.id2 == "CaseObject")
    val Some(caseObjectParent: models.RefModel) = caseObject.parent
    assertEquals(caseObjectParent.id2, "MixedSealed")
    val Some(caseObjectInnerModel) = caseObject.allOf.find(_.id2 == "CaseObject")

    assertEquals(caseClassInnerModel.properties, caseObjectInnerModel.properties)
  }

  test("A TypeBuilder should build a model for a case class containing a sealed enum") {
    val ms = modelOf[SealedEnumContainer]
    assertEquals(ms.size, 1)
    val sealedEnumContainerModel = ms.head
    val e = sealedEnumContainerModel.properties("e")
    val enumProperty = e.asInstanceOf[StringProperty]
    assertEquals(enumProperty.enums, Set("FooEnum", "BarEnum"))
  }

  test("A TypeBuilder should build a model for a type recursive through sealed trait") {
    assertEquals(modelOf[BTree].size, 3)
  }

  test("A TypeBuilder should build a model for a type recursive through Option") {
    assertEquals(modelOf[BarList].size, 1)
  }

  test("A TypeBuilder should build a model for a type recursive through Either") {
    assertEquals(modelOf[FooEither].size, 1)
  }

  test("A TypeBuilder should build a model for a class using type parameters of an outer class") {
    val ms = modelOf[OuterInt.Inner]
    assertEquals(ms.size, 1)
    val m = ms.head
    val t = m.properties.get("t")
    assert(t.nonEmpty)
    assertEquals(t.get.`type`, "integer")
    assertEquals(t.get.format, Some("int32"))
  }

  test("A TypeBuilder should build a model for shapeless coproduct (:+:)") {
    val ms = modelOf[ShapelessIntOrString]
    assert(ms.nonEmpty)
    // It won't be a fully correct model.
  }

  test(
    "A TypeBuilder should format names of generic types according to provided ShowType instance"
  ) {
    implicit val showType: ShowType = new ShowTypeWithBrackets("<", ";", ">")

    val ms = modelOf[FooDoubleGeneric[Int, String]]
    assertEquals(ms.size, 1)
    val m = ms.head
    assertEquals(m.description, "FooDoubleGeneric<Int;String>".some)
  }

  test("A DataType should get the correct name for primitives") {
    assertEquals(DataType(typeTag[Int]).name, "integer")
    assertEquals(DataType(typeTag[String]).name, "string")
  }

  test("A DataType should get the type for a container") {
    assertEquals(DataType(typeTag[Seq[Int]]).name, "List")
  }

  test("A DataType should get the DataType of the underlying type for an AnyVal") {
    val datatypes = List[(TypeTag[_], TypeTag[_])](
      (typeTag[StringAnyVal], typeTag[String]),
      (typeTag[IntAnyVal], typeTag[Int]),
      (typeTag[DoubleAnyVal], typeTag[Double]),
      (typeTag[EntityAnyVal], typeTag[Entity])
    )

    assert(datatypes.forall { case (anyValType, underlyingType) =>
      DataType(anyValType) == DataType(underlyingType)
    })
  }

  test("A DataType should get the DataType for sealed enums") {
    assertEquals(DataType(typeTag[SealedEnum]), DataType.EnumDataType(Set("BarEnum", "FooEnum")))
  }
}

case class StringAnyVal(value: String) extends AnyVal
case class IntAnyVal(value: Int) extends AnyVal
case class DoubleAnyVal(value: Double) extends AnyVal
case class EntityAnyVal(value: Entity) extends AnyVal

case class Entity(name: String)
