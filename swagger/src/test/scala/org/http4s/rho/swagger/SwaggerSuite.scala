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

import munit.ScalaCheckSuite
import org.http4s.rho.swagger.models.Swagger
import org.scalacheck.Prop._

import scala.jdk.CollectionConverters._

import Arbitraries._

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
