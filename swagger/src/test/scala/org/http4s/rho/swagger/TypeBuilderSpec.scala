package org.http4s.rho.swagger

import java.sql.Timestamp
import java.util.Date

import com.wordnik.swagger.model.Model
import org.specs2.mutable.Specification

import scala.reflect.runtime.universe.{TypeTag, typeTag}

case class Foo(a: Int, b: String)
case class FooDefault(a: Int = 0)
case class FooGeneric[+A](a: A)
case class Bar(foo: Foo, a: Int)
case class FooWithList(l: List[Int])
case class FooWithMap(l: Map[String,Int])


class TypeBuilderSpec extends Specification {

  def models[T](implicit t: TypeTag[T]): Set[Model] = TypeBuilder.collectModels(t, Set.empty)

  "TypeBuilder" should {

    "Not Build a model for a primitive" in {

      val primitives = {
        Set[TypeTag[_]](typeTag[String], typeTag[Int], typeTag[Long], typeTag[Double],
          typeTag[Float], typeTag[Byte], typeTag[BigInt], typeTag[Boolean],
          typeTag[Short], typeTag[java.lang.Integer], typeTag[java.lang.Long],
          typeTag[java.lang.Double], typeTag[java.lang.Float], typeTag[BigDecimal],
          typeTag[java.lang.Byte], typeTag[java.lang.Boolean], typeTag[Number],
          typeTag[java.lang.Short], typeTag[Date], typeTag[Timestamp], typeTag[Symbol],
          typeTag[java.math.BigDecimal], typeTag[java.math.BigInteger])
      }

      val models = primitives.foldLeft(Set.empty[Model])((s, t) => TypeBuilder.collectModels(t, s))

      models.isEmpty must_== true
    }

    "Build simple model" in {
      val model = models[Foo].head
      model.id must_== "Foo"
      model.properties("a").`type` must_== "integer"
      model.properties("a").required must_== true
    }

    "Build a model with a default" in {
      val ms = models[FooDefault]
      ms.size must_== 1

      val m  = ms.head

      m.name must_== "FooDefault"
      m.properties.size must_== 1
      m.properties("a").`type` must_== "integer"
      m.properties("a").required must_== false
    }

    "Build a model with a generic" in {
      val ms = models[FooGeneric[Int]]
      ms.size must_== 1
      val m = ms.head
      m.id must_== "FooGeneric[Int]"

      m.properties.size must_== 1
      m.properties("a").`type` must_== "integer"
    }

    "Build a model with a non-basic generic" in {
      val ms = models[FooGeneric[Foo]]
      ms.size must_== 2
      ms ++ models[Foo] must_== ms
    }

    "Get types from a collection" in {
      import scalaz.concurrent.Task
      import scalaz.stream.Process

      models[Seq[Foo]] must_== models[Foo]
      models[Map[String,Foo]] must_== models[Foo]
      models[Process[Task,Foo]] must_== models[Foo]
    }

    "Build model that contains a List" in {
      val ms = models[FooWithList]
      ms.size must_== 1
      val m = ms.head
      m.id must_== "FooWithList"

      val p = m.properties.head._2
      p.`type` must_== "List"
      p.items.isDefined must_== true

      p.items.get.`type` must_== "integer"
    }
  }

  "DataType" should {
    import org.http4s.rho.swagger.TypeBuilder.DataType

    "Get the correct name for primitives" in {
      DataType(typeTag[Int]).name must_== "integer"
      DataType(typeTag[String]).name must_== "string"
    }

    "Get the type for a container" in {
      println(DataType(typeTag[Seq[Int]]))
      DataType(typeTag[Seq[Int]]).name must_== "List"
    }
  }

}
