/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s
package rho

import cats.Functor
import shapeless.HList

import scala.reflect.runtime.universe.TypeTag

/** Type that can accept a decoder.
  *
  * A subtype of [[Decodable]] can accept an `EntityDecoder` for extracting a message from the
  * `Request` body.
  */
trait Decodable[F[_], T <: HList, R] {

  /** Decode the body using the `EntityDecoder`
    *
    * Alias for the `^` operator.
    *
    * @param decoder
    *   `EntityDecoder` to utilize for decoding the body.
    * @tparam R2
    *   type of the result.
    */
  def decoding[R2 >: R](
      decoder: EntityDecoder[F, R2])(implicit F: Functor[F], t: TypeTag[R2]): CodecRouter[F, T, R2]

  /** Decode the body using the `EntityDecoder`
    *
    * Alias for decoding
    * @param decoder
    *   `EntityDecoder` to utilize for decoding the body.
    * @tparam R2
    *   type of the result.
    */
  final def ^[R2 >: R](decoder: EntityDecoder[F, R2])(implicit
      F: Functor[F],
      t: TypeTag[R2]): CodecRouter[F, T, R2] = decoding(decoder)
}
