package org.http4s
package rho

import bits.EntityParser

import scala.reflect.runtime.universe.TypeTag

import shapeless.HList

/** Type that can accept a parser.
  *
  * A subtype of [[Decodable]] can accept an `EntityParser` for extracting a message
  * from the `Request` body.
  */
trait Decodable[T <: HList, R] {

  /** Decode the body using the `EntityParser`
    *
    * Alias for the `^` operator.
    *
    * @param parser `EntityParser` to utilize for decoding the body.
    * @tparam R2 type of the result.
    */
  def decoding[R2 >: R](parser: EntityParser[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2]

  /** Decode the body using the `EntityParser`
    *
    * Alias for decoding
    * @param parser `EntityParser` to utilize for decoding the body.
    * @tparam R2 type of the result.
    */
  final def ^[R2 >: R](parser: EntityParser[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2] = decoding(parser)
}
