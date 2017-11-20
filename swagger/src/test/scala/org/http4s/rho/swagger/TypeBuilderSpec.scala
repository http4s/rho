package org.http4s.rho.swagger

import java.sql.Timestamp
import java.util.Date

import org.http4s.rho.swagger.models.AbstractProperty
import org.specs2.execute.Result
import org.specs2.mutable.Specification

import fs2.{IO, Stream}
import scala.reflect.runtime.universe.{TypeTag, typeOf, typeTag}
import cats.syntax.all._

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
  case class FooVal(str: String) extends AnyVal
  case class BarWithFooVal(fooVal: FooVal)
  @DiscriminatorField("foobar")
  sealed trait Sealed {
    def foo: String
  }
  case class FooSealed(a: Int, foo: String, foo2: Foo) extends Sealed
  case class BarSealed(str: String, foo: String) extends Sealed

  case class ContainsSealed(seal: Sealed)

  sealed trait SealedEnum
  case object FooEnum extends SealedEnum
  case object BarEnum extends SealedEnum
}

class TypeBuilderSpec extends Specification {
  import model._
  import models.{ArrayProperty, Model, RefProperty}

  def modelOf[T](implicit t: TypeTag[T]): Set[Model] =
    modelOfWithFormats(DefaultSwaggerFormats)

  def modelOfWithFormats[T](formats: SwaggerFormats)(implicit t: TypeTag[T]): Set[Model] =
    TypeBuilder.collectModels(t.tpe, Set.empty, formats)

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
        TypeBuilder.collectModels(t.tpe, s, DefaultSwaggerFormats)
      }

      ms.isEmpty must_== true
    }

    "Identify types" in {
      typeOf[IO[String]].isIO must_== true
      typeOf[String].isIO must_== false

      typeOf[Stream[IO,String]].isStream must_== true
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
      ms.size must_== 1
      val m = ms.head
      m.description must_== "FooGeneric«Nothing»".some

      m.properties.size must_== 1
      m.properties.get("a").get.`type` must_== "void"
    }

    "Build a model with a generic of type Null" in {
      val ms = modelOf[FooGeneric[Null]]
      ms.size must_== 1
      val m = ms.head
      m.description must_== "FooGeneric«Null»".some

      m.properties.size must_== 1
      m.properties.get("a").get.`type` must_== "void"
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
      m1.properties.get("single").get.`type` must_== "Foo"
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
      import fs2.{IO, Stream}
      modelOf[Stream[IO, Foo]] must_== modelOf[Foo]
    }

    "Get types from a cats.effect.IO" in {
      import cats.effect.IO
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

    "Build an empty model for an AnyVal" in {
      val ms = modelOf[FooVal]
      ms must be empty
    }

    "Build a model with the underlying type for AnyVals" in {
      val ms = modelOf[BarWithFooVal]

      ms.size must_== 1
      val m = ms.head

      m.properties.size must_== 1
      m.properties.head must beLike {
        case (name, prop) =>
          name must_== "fooVal"
          prop.`type` must_== "string"
      }
    }

    "Build a model for sealed traits" in {
      val ms = modelOf[Sealed]
      ms.foreach(_.toJModel) // Testing that there are no exceptions
      ms.size must_== 4
      val Some(seal: models.ModelImpl) = ms.find(_.id2 == "Sealed")
      seal.discriminator must_== Some("foobar")
      val Some(foo: models.ComposedModel) = ms.find(_.id2 == "FooSealed")
      val Some(fooRef) = foo.allOf.collectFirst {case ref: models.RefModel => ref}
      fooRef.ref must_== "Sealed"
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
