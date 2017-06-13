package org.http4s.rho.swagger

import scala.reflect.runtime.universe._

import models._

final case class SwaggerFormats(customSerializers: PartialFunction[Type, Set[Model]],
                                customFieldSerializers: PartialFunction[Type, Property]) {

  def withSerializers(serializer: PartialFunction[Type, Set[Model]]): SwaggerFormats =
    this.copy(customSerializers = serializer orElse this.customSerializers)

  def withSerializers(t: Type, models: Set[Model]): SwaggerFormats = withSerializers {
    case tpe if tpe =:= t => models
  }

  def withFieldSerializers(fieldSerializer: PartialFunction[Type, Property]): SwaggerFormats =
    this.copy(customFieldSerializers = fieldSerializer orElse this.customFieldSerializers)

  def withFieldSerializers(t: Type, property: Property): SwaggerFormats = withFieldSerializers {
      case tpe if tpe =:= t => property
  }
}

object SwaggerFormats {
  private type F = PartialFunction[Type, Set[Model]]

  private val ignoreExistentialType: F = {
    case ExistentialType(_, _) => Set.empty
  }

  private val ignoreNothingOrNull: F = {
    case tpe if tpe.isNothingOrNull => Set.empty
  }

  val emptySerializers: PartialFunction[Type, Set[Model]] = PartialFunction.empty

  val emptyFieldSerializers: PartialFunction[Type, Property] = PartialFunction.empty

  val defaultSwaggerFormats: SwaggerFormats =
    SwaggerFormats(ignoreNothingOrNull orElse ignoreExistentialType, emptyFieldSerializers)

  val emptySwaggerFormats: SwaggerFormats =
    SwaggerFormats(emptySerializers, emptyFieldSerializers)

  def withSerializers(serializer: PartialFunction[Type, Set[Model]]): SwaggerFormats = {
    SwaggerFormats(serializer, emptyFieldSerializers)
  }

  def withFieldSerializers(fieldSerializer: PartialFunction[Type, Property]): SwaggerFormats = {
    SwaggerFormats(emptySerializers, fieldSerializer)
  }
}



