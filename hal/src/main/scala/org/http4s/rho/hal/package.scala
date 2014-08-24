package org.http4s.rho

import org.http4s.rho.hal.LinkObject
import org.http4s.rho.hal.ResourceObject

/** Describes Hypertext Application Language types and functions */
package object hal {

  type EmbeddedDef[T] = (String, Either[ResourceObject[T, _], Seq[ResourceObject[T, _]]])

  type Embedded[T] = List[EmbeddedDef[T]]

  type LinkObjectDef = (String, Either[LinkObject, Seq[LinkObject]])

  type Links = List[LinkObjectDef]

}
