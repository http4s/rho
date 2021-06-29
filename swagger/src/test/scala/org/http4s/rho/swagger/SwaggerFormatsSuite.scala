package org.http4s.rho.swagger

import cats.effect.IO

import cats.syntax.all._
import munit.FunSuite
import scala.collection.immutable.Seq
import scala.reflect.runtime.universe._

class SwaggerFormatsSuite extends FunSuite {

  object model {
    sealed trait Fruit
    case object Apple extends Fruit
    case object Pear extends Fruit
    case object Cherry extends Fruit
    case class FruitBox(fruits: Seq[Fruit])
  }

  import model._
  import models._

  implicit val showType: ShowType = DefaultShowType

  test("SwaggerFormats.withSerializers") {
    val m = ModelImpl(id = "fruit-box", id2 = "fruit-box", description = "model.FruitBox".some)
    val sfs = DefaultSwaggerFormats.withSerializers(typeOf[FruitBox], Set(m))
    val itag = implicitly[TypeTag[IO[_]]]

    def modelOf[T](t: TypeTag[T]): Set[Model] =
      TypeBuilder.collectModels(t.tpe, Set.empty, sfs, itag.tpe)

    assert(modelOf(typeTag[FruitBox]).nonEmpty)
    assertEquals(modelOf(typeTag[FruitBox]).head.id, "fruit-box")
    assertEquals(modelOf(typeTag[FruitBox]).head.description, "model.FruitBox".some)
    assert(modelOf(typeTag[FruitBox]).head.properties.isEmpty)
  }

  test("SwaggerFormats.withFieldSerializers") {
    val arrProp =
      ArrayProperty(items = RefProperty("Fruit"), required = true, uniqueItems = false)
    val sfs = DefaultSwaggerFormats.withFieldSerializers(typeOf[Seq[Fruit]], arrProp)
    val itag = implicitly[TypeTag[IO[_]]]

    def modelOf[T](t: TypeTag[T]): Set[Model] =
      TypeBuilder.collectModels(t.tpe, Set.empty, sfs, itag.tpe)

    assert(modelOf(typeTag[FruitBox]).nonEmpty)
    assertEquals(modelOf(typeTag[FruitBox]).head.properties.head._1, "fruits")
    assertEquals(modelOf(typeTag[FruitBox]).head.properties.head._2, arrProp)
  }
}
