package org.http4s
package rho.bits

trait Metadata

case class TextMeta(id: String, msg: String) extends Metadata

trait MetaDataSyntax {
  type Self

  def addMetaData(data: Metadata): Self

  final def ^(id: String, desc: String): Self = description(id, desc)

  final def description(id: String, desc: String) = addMetaData(TextMeta(id, desc))
}
