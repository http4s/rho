package org.http4s
package rho

import bits.PathAST._

trait MetaDataSyntax {
  type Self

  protected def addMetaData(data: MetaData): Self

  final def ^(desc: String): Self = description(desc)

  final def description(desc: String) = addMetaData(PathDescription(desc))
}
