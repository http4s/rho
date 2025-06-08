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

import scala.reflect.runtime.universe._

import models._

final case class SwaggerFormats(
    customSerializers: PartialFunction[Type, Set[Model]],
    customFieldSerializers: PartialFunction[Type, Property]) {

  def withSerializers(serializer: PartialFunction[Type, Set[Model]]): SwaggerFormats =
    this.copy(customSerializers = serializer.orElse(this.customSerializers))

  def withSerializers(t: Type, models: Set[Model]): SwaggerFormats = withSerializers {
    case tpe if tpe =:= t => models
  }

  def withFieldSerializers(fieldSerializer: PartialFunction[Type, Property]): SwaggerFormats =
    this.copy(customFieldSerializers = fieldSerializer.orElse(this.customFieldSerializers))

  def withFieldSerializers(t: Type, property: Property): SwaggerFormats = withFieldSerializers {
    case tpe if tpe =:= t => property
  }
}

object SwaggerFormats {
  val emptySerializers: PartialFunction[Type, Set[Model]] = PartialFunction.empty

  val emptyFieldSerializers: PartialFunction[Type, Property] = PartialFunction.empty
}
