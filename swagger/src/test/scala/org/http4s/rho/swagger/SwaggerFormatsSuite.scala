/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
    assertEquals(modelOf(typeTag[FruitBox]).head.properties, Map.empty[String, Property])
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
