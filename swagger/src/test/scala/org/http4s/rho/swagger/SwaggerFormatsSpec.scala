package org.http4s.rho.swagger

import scala.reflect.runtime.universe._

import org.specs2.mutable.Specification

import scalaz._, Scalaz._

class SwaggerFormatsSpec extends Specification {

  object model {
    sealed trait Fruit
    case object Apple extends Fruit
    case object Pear extends Fruit
    case object Cherry extends Fruit
    case class FruitBox(fruits: Seq[Fruit])
  }

  import model._
  import models._

  "SwaggerFormats" should {

    "withSerializers" in {
      val m = ModelImpl(id = "fruit-box", id2 = "fruit-box", description = "model.FruitBox".some)
      val sfs = SwaggerFormats.defaultSwaggerFormats.withSerializers(typeOf[FruitBox], Set(m))

      def modelOf[T](t: TypeTag[T]): Set[Model] =
        TypeBuilder.collectModels(t.tpe, Set.empty, sfs)

      modelOf(typeTag[FruitBox]).nonEmpty must_== true
      modelOf(typeTag[FruitBox]).head.id must_== "fruit-box"
      modelOf(typeTag[FruitBox]).head.description must_== "model.FruitBox".some
      modelOf(typeTag[FruitBox]).head.properties.isEmpty must_== true
    }

    "withFieldSerializers" in {
      val arrProp = ArrayProperty(items = RefProperty("Fruit"), required = true, uniqueItems = false)
      val sfs = SwaggerFormats.defaultSwaggerFormats.withFieldSerializers(typeOf[Seq[Fruit]], arrProp)

      def modelOf[T](t: TypeTag[T]): Set[Model] =
        TypeBuilder.collectModels(t.tpe, Set.empty, sfs)

      modelOf(typeTag[FruitBox]).nonEmpty must_== true
      modelOf(typeTag[FruitBox]).head.properties.head._1 must_== "fruits"
      modelOf(typeTag[FruitBox]).head.properties.head._2 must_== arrProp
    }
  }
}
