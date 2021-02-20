package org.http4s.rho.swagger

import org.specs2.mutable._
import org.specs2.ScalaCheck
import Arbitraries._
import io.swagger.models.{Response => jResponse}
import org.http4s.rho.swagger.models.Swagger

import scala.jdk.CollectionConverters._

class SwaggerSpec extends Specification with ScalaCheck {
  "The Swagger model can be translated to a 'Java' Swagger model".p

  "If there are no examples in the path responses then the corresponding field in the Java model must be null" >> prop {
    swagger: Swagger =>
      val paths = swagger.paths.values.toList
      val operations = paths.flatMap(_.operations)
      val responses = operations.flatMap(_.responses.values.toList)
      val examples = responses.flatMap(_.examples)

      val jpaths = swagger.toJModel.getPaths.asScala.values.toList
      val joperations = jpaths.flatMap(_.getOperations.asScala)
      val jresponses = joperations.flatMap(_.getResponses.asScala.values.toList)

      jresponses must contain { response: jResponse =>
        response.getExamples must beNull.iff(examples.isEmpty)
      }.forall
  }

}
