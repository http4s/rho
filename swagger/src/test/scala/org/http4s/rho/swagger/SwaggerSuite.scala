package org.http4s.rho.swagger

import Arbitraries._
import munit.ScalaCheckSuite
import org.http4s.rho.swagger.models.Swagger
import org.scalacheck.Prop._

import scala.jdk.CollectionConverters._

class SwaggerSuite extends ScalaCheckSuite {
  property(
    "The Swagger model can be translated to a 'Java' Swagger model. " +
      "If there are no examples in the path responses then the corresponding field in the Java model must be null"
  ) {
    forAll { swagger: Swagger =>
      val paths = swagger.paths.values.toList
      val operations = paths.flatMap(_.operations)
      val responses = operations.flatMap(_.responses.values.toList)
      val examples = responses.flatMap(_.examples)

      val jpaths = swagger.toJModel.getPaths.asScala.values.toList
      val joperations = jpaths.flatMap(_.getOperations.asScala)
      val jresponses = joperations.flatMap(_.getResponses.asScala.values.toList)

      assert(
        jresponses.forall(response =>
          if (examples.isEmpty) response.getExamples == null
          else true
        )
      )
    }
  }
}
