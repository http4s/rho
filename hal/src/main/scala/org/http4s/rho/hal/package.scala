package org.http4s.rho

import org.http4s.rho.hal.LinkObject
import org.http4s.rho.hal.ResourceObject

/** Describes Hypertext Application Language types and functions */
package object hal {

  type EmbeddedDef = (String, Entry[ResourceObject[_]])

  type Embedded = Vector[EmbeddedDef]

  type LinkObjectDef = (String, Entry[LinkObjectLike])

  type Links = Vector[LinkObjectDef]

  sealed abstract class Entry[+A]
  final case class Single[+A](x: A) extends Entry[A]
  final case class Many[+A](xs: Vector[A]) extends Entry[A]
  object Many {
    def empty[A]: Many[A] = Many()
    def apply[A](xs: A*) = new Many(xs.toVector)
  }

}
