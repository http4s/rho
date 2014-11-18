package org.http4s.rho.swagger

import scala.collection.mutable
import scala.reflect.runtime.universe._

import org.specs2.mutable.Specification

import com.wordnik.swagger.model.Model
import com.wordnik.swagger.model.ModelProperty



class SwaggerFormatsSpec extends Specification {

  object model {
    sealed trait Fruit
    case object Apple extends Fruit
    case object Pear extends Fruit
    case object Cherry extends Fruit
    case class FruitBox(fruits: Seq[Fruit])
  }

  import model._

  "SwaggerFormats" should {

    "withFieldSerializers" in {
      val swaggerFormats = DefaultSwaggerFormats.withFieldSerializers(typeOf[Seq[Fruit]], ModelProperty("List", "array«Fruit»"))
      def models[T](t: TypeTag[T]): Set[Model] = TypeBuilder.collectModels(t.tpe, Set.empty, swaggerFormats)
      models(typeTag[FruitBox]).nonEmpty must_== true
      models(typeTag[FruitBox]).head.properties.head._1 must_== "fruits"

      // following assertion fails
      models(typeTag[FruitBox]).head.properties.head._2 must_== ModelProperty("List", "array«Fruit»")
    }
  }
}
