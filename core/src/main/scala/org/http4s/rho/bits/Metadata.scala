package org.http4s
package rho.bits

import scala.language.existentials

import scala.reflect.runtime.universe.TypeTag

/** Base type for data that can be used to decorate the rules trees
  *
  * Metadata is not directly useful for evaluating a request or for generating
  * HTTP responses; it is primarily used for storing data to auto generate information
  * about a route or service.
  */
trait Metadata

/** Textual meta data */
trait TextMetaData extends Metadata {
  def msg: String
}

/** Metadata about a query rule */
case class QueryMetaData[T](name: String, p: QueryParser[T], default: Option[T], m: TypeTag[T]) extends Metadata

/** Metadata about a header rule */
case class HeaderMetaData[T <: HeaderKey.Extractable](key: T, default: Boolean) extends Metadata