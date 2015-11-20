package org.http4s
package rho.bits

import scala.language.existentials

import scala.reflect.runtime.universe.TypeTag

trait Metadata

trait TextMetaData extends Metadata {
  def msg: String
}

case class QueryMetaData[T](name: String, p: QueryParser[T], default: Option[T], m: TypeTag[T]) extends Metadata

case class HeaderMetaData[T <: HeaderKey.Extractable](key: T, default: Boolean) extends Metadata