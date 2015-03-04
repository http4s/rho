package org.http4s.rho

import org.http4s.EntityDecoder

import scala.reflect.runtime.universe.{Type, TypeTag}

import shapeless.HList

trait Decodable[T <: HList, R] {

  /** Decode the body using the `EntityDecoder` */
  def decoding[R2 >: R](decoder: EntityDecoder[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2]

  /** Alias for decoding */
  final def ^[R2 >: R](decoder: EntityDecoder[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2] = decoding(decoder)
}
