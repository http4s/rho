package org.http4s.rho.swagger

import java.sql.Timestamp
import java.util.Date

import org.specs2.scalaz.{Spec, ScalazMatchers}

import scalaz.concurrent.Task
import scalaz.stream.Process

import scala.reflect.runtime.universe.{ TypeTag, typeTag, typeOf }

import scalaz._, Scalaz._

package object model {
  case class Foo(a: Int, b: String)
  type Bar = Foo
  case class FooDefault(a: Int = 0)
  case class FooGeneric[+A](a: A)
  type Foos = Seq[Foo]
  case class FooComposite(single: Foo, many: Seq[Foo])
  case class FooCompositeWithAlias(single: Bar, many: Seq[Bar], manyAlias: Foos)
  case class FooWithList(l: List[Int])
  case class FooWithMap(l: Map[String, Int])
}

class TypeBuilderSpec extends Spec {
  import model._
  import models.Model

  def modelOf[T](implicit t: TypeTag[T]): Set[Model] =
    TypeBuilder.collectModels(t.tpe, Set.empty, DefaultSwaggerFormats)

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
      typeOf[Task[String]].isTask must_== true
      typeOf[String].isTask must_== false

      typeOf[Process[Task,String]].isProcess must_== true
      typeOf[String].isProcess must_== false

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
      m1.properties.get("many").get.`type` must_== "array"

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

    "Get types from a collection" in {
      modelOf[Seq[Foo]] must_== modelOf[Foo]
      modelOf[Map[String, Foo]] must_== modelOf[Foo]
    }

    "Get types from a scalaz.stream.Process" in {
      import scalaz.concurrent.Task
      import scalaz.stream.Process
      modelOf[Process[Task, Foo]] must_== modelOf[Foo]
    }

    "Get types from a scalaz.concurrent.Task" in {
      import scalaz.concurrent.Task
      modelOf[Task[Foo]] must_== modelOf[Foo]
    }

    "Build model that contains a List" in {
      val ms = modelOf[FooWithList]
      ms.size must_== 1
      val m = ms.head
      m.description must_== "FooWithList".some

      //val p = m.properties.head._2
      // p.`type` must_== "List"
      // p.items.isDefined must_== true

      // p.items.get.`type` must_== "integer"
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
  }
}
