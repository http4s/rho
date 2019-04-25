package org.http4s.rho

import scala.collection.immutable.Seq

/** Describes Hypertext Application Language types and functions */
package object hal {

  type EmbeddedDef[T] = (String, Either[ResourceObject[T, _], Seq[ResourceObject[T, _]]])

  type Embedded[T] = List[EmbeddedDef[T]]

  type LinkObjectDef = (String, Either[LinkObject, Seq[LinkObject]])

  type Links = List[LinkObjectDef]

}
