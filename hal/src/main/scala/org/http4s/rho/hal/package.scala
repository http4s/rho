package org.http4s.rho

import org.http4s.rho.hal.LinkObject
import org.http4s.rho.hal.ResourceObject

/** Describes Hypertext Application Language types and functions */
package object hal {

  type EmbeddedDef = (String, Entry[ResourceObject[_]])

  type Embedded = List[EmbeddedDef]

  type LinkObjectDef = (String, Entry[LinkObjectLike])

  type Links = List[LinkObjectDef]

  sealed trait Entry[+A]
  final case class Single[+A](x: A) extends Entry[A]
  final case class Many[+A](xs: List[A] = Nil) extends Entry[A]
  object Many {
    def empty[A]: Many[A] = Many()
    def apply[A](xs: A*) = new Many(xs.toList)
  }

}
