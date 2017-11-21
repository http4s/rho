package org.http4s
package rho

import cats.effect.IO

import scala.reflect.runtime.universe.TypeTag
import shapeless.HList

/** Type that can accept a decoder.
  *
  * A subtype of [[Decodable]] can accept an `EntityDecoder` for extracting a message
  * from the `Request` body.
  */
trait Decodable[T <: HList, R] {

  /** Decode the body using the `EntityDecoder`
    *
    * Alias for the `^` operator.
    *
    * @param decoder `EntityDecoder` to utilize for decoding the body.
    * @tparam R2 type of the result.
    */
  def decoding[R2 >: R](decoder: EntityDecoder[IO, R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2]

  /** Decode the body using the `EntityDecoder`
    *
    * Alias for decoding
    * @param decoder `EntityDecoder` to utilize for decoding the body.
    * @tparam R2 type of the result.
    */
  final def ^[R2 >: R](decoder: EntityDecoder[IO, R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2] = decoding(decoder)
}
