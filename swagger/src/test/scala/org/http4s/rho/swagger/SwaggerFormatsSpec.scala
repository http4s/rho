// package org.http4s.rho.swagger

// import scala.collection.mutable.LinkedHashMap
// import scala.reflect.runtime.universe._

// import org.specs2.mutable.Specification

// import com.wordnik.swagger.model.Model
// import com.wordnik.swagger.model.ModelProperty

// class SwaggerFormatsSpec extends Specification {

//   object model {
//     sealed trait Fruit
//     case object Apple extends Fruit
//     case object Pear extends Fruit
//     case object Cherry extends Fruit
//     case class FruitBox(fruits: Seq[Fruit])
//   }

//   import model._

//   "SwaggerFormats" should {

//     "withSerializers" in {
//       val swaggerFormats = DefaultSwaggerFormats.withSerializers(typeOf[FruitBox], Set(Model("fruit-box", "FruitBox", "model.FruitBox", LinkedHashMap.empty[String, ModelProperty])))
//       def models[T](t: TypeTag[T]): Set[Model] = TypeBuilder.collectModels(t.tpe, Set.empty, swaggerFormats)
//       models(typeTag[FruitBox]).nonEmpty must_== true
//       models(typeTag[FruitBox]).head.id must_== "fruit-box"
//       models(typeTag[FruitBox]).head.name must_== "FruitBox"
//       models(typeTag[FruitBox]).head.qualifiedType must_== "model.FruitBox"
//       models(typeTag[FruitBox]).head.properties.isEmpty must_== true
//     }

//     "withFieldSerializers" in {
//       val swaggerFormats = DefaultSwaggerFormats.withFieldSerializers(typeOf[Seq[Fruit]], ModelProperty("List", "array«Fruit»"))
//       def models[T](t: TypeTag[T]): Set[Model] = TypeBuilder.collectModels(t.tpe, Set.empty, swaggerFormats)
//       models(typeTag[FruitBox]).nonEmpty must_== true
//       models(typeTag[FruitBox]).head.properties.head._1 must_== "fruits"
//       models(typeTag[FruitBox]).head.properties.head._2 must_== ModelProperty("List", "array«Fruit»")
//     }
//   }
// }
