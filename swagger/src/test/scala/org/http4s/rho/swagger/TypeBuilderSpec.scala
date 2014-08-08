package org.http4s.rho.swagger

import org.specs2.mutable.Specification

import scala.reflect.runtime.universe.TypeTag

case class Foo[+A](a: A, b: String = "")
case class Bar(foo: Foo[String], a: Int)


class TypeBuilderSpec extends Specification {

  "TypeBuilder" should {
    "Work" in {
      val models = TypeBuilder.collectModels(implicitly[TypeTag[List[Bar]]], Set.empty)
      println(models)
      true must_== true
    }
  }

}
