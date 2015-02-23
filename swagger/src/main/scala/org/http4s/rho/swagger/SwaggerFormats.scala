package org.http4s.rho.swagger

import scala.reflect.runtime.universe._

import models._

sealed trait SwaggerFormats { self =>

  def customSerializers: PartialFunction[Type, Set[Model]] = PartialFunction.empty
  def customFieldSerializers: PartialFunction[Type, Property] = PartialFunction.empty

  /** Construct a new SwaggerFormats with custom model serializers */
  def withSerializers(s: PartialFunction[Type, Set[Model]]): SwaggerFormats = new SwaggerFormats {
    override val customSerializers: PartialFunction[Type, Set[Model]] = s orElse self.customSerializers
    override val customFieldSerializers: PartialFunction[Type, Property] = self.customFieldSerializers
  }

  /** Construct a new SwaggerFormats with custom model serializers */
  def withSerializers(t: Type, ms: Set[Model]): SwaggerFormats = withSerializers {
    case tpe if tpe =:= t => ms
  }

  /** Construct a new SwaggerFormats with custom field serializers */
  def withFieldSerializers(f: PartialFunction[Type, Property]): SwaggerFormats = new SwaggerFormats {
    override val customSerializers: PartialFunction[Type, Set[Model]] = self.customSerializers
    override val customFieldSerializers: PartialFunction[Type, Property] = f orElse self.customFieldSerializers
  }

  /** Construct a new SwaggerFormats with custom field serializers */
  def withFieldSerializers(t: Type, ms: Property): SwaggerFormats = withFieldSerializers {
    case tpe if tpe =:= t => ms
  }
}

object DefaultSwaggerFormats extends SwaggerFormats {

  type F = PartialFunction[Type, Set[Model]]

  val ignoreExistentialType: F = {
    case ExistentialType(_, _) => Set.empty
  }

  val ignoreNothingOrNull: F = {
    case tpe if tpe.isNothingOrNull => Set.empty
  }

  override def customSerializers = ignoreNothingOrNull orElse ignoreExistentialType
}
