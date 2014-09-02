package org.http4s.rho.swagger

import com.wordnik.swagger.model._

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe._

/**
 * Formats to use when converting to [[Model]].
 * Formats are usually configured by using an implicit parameter:
 * <pre>
 * implicit val formats = org.http4s.rho.DefaultFormats
 * </pre>
 */
@implicitNotFound(
  "No org.http4s.rho.Formats found. Try to bring an instance of org.http4s.rho.Formats in scope or use the org.http4s.rho.DefaultFormats.")
trait Formats { self: Formats =>
  def customSerializers: List[PartialFunction[Type, Set[Model]]] = Nil
  def customFieldSerializers: List[PartialFunction[Type, Set[ModelProperty]]] = Nil
}

object DefaultFormats extends Formats {

  type F = PartialFunction[Type, Set[Model]]

  val ignoreExistentialType: F = {
    case ExistentialType(_, _) => Set.empty
  }

  val ignoreNothingOrNull: F = {
    case tpe if tpe.isNothingOrNull => Set.empty
  }

  override def customSerializers =
    ignoreNothingOrNull ::
      ignoreExistentialType ::
      Nil
}
