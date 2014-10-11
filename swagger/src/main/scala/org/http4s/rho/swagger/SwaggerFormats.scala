package org.http4s.rho.swagger

import com.wordnik.swagger.model._
import org.json4s.JsonAST.JValue
import scala.reflect.runtime.universe._

/**
 * Formats to use when converting to [[Model]].
 * Formats are usually configured by using an implicit parameter:
 * <pre>
 * implicit val formats = org.http4s.rho.DefaultFormats
 * </pre>
 */
sealed trait SwaggerFormats { self =>
  def customSerializers: PartialFunction[Type, Set[Model]] = PartialFunction.empty
  def customFieldSerializers: PartialFunction[Type, Set[ModelProperty]] = PartialFunction.empty

  /** Construct a new SwaggerFormats with custom model serializers */
  def withSerializers(s: PartialFunction[Type, Set[Model]]): SwaggerFormats = new SwaggerFormats {
    override val customSerializers: PartialFunction[Type, Set[Model]] = s orElse self.customSerializers
    override val customFieldSerializers: PartialFunction[Type, Set[ModelProperty]] = self.customFieldSerializers
  }

  /** Construct a new SwaggerFormats with custom model serializers */
  def withSerializers(t: Type, ms: Set[Model]): SwaggerFormats = withSerializers {
    case tpe if tpe =:= t => ms
  }

  /** Construct a new SwaggerFormats with custom field serializers */
  def withFieldSerializers(f: PartialFunction[Type, Set[ModelProperty]]): SwaggerFormats = new SwaggerFormats {
    override val customSerializers: PartialFunction[Type, Set[Model]] = self.customSerializers
    override val customFieldSerializers: PartialFunction[Type, Set[ModelProperty]] = f orElse self.customFieldSerializers
  }

  /** Construct a new SwaggerFormats with custom field serializers */
  def withFieldSerializers(t: Type, ms: Set[ModelProperty]): SwaggerFormats = withFieldSerializers {
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

  val jsonSerializers: F = {
    case tpe if tpe <:< typeOf[JValue] => Set.empty
  }

  override def customSerializers = jsonSerializers orElse ignoreNothingOrNull orElse ignoreExistentialType
}
