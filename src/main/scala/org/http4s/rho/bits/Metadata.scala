package org.http4s
package rho.bits


trait Metadata

case class TextMeta(msg: String) extends Metadata


trait MetaDataSyntax {
  type Self

  def addMetaData(data: Metadata): Self

  final def ^(desc: String): Self = description(desc)

  final def description(desc: String) = addMetaData(TextMeta(desc))
}