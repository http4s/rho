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
package rho.bits

import shapeless.HList
import shapeless.ops.hlist.Prepend

/** Base trait which is capable of appending header rules
  * @tparam T The `HList` representation of the values to be extracted from the `Request`.
  */
trait HeaderAppendable[F[_], T <: HList] {
  type HeaderAppendResult[T0 <: HList] <: HeaderAppendable[F, T0]

  /** Append the header to the builder, generating a new typed representation of the route */
  def >>>[T1 <: HList](header: TypedHeader[F, T1])(implicit
      prep: Prepend[T1, T]): HeaderAppendResult[prep.Out]

  /** Append the header to the builder, generating a new typed representation of the route */
  final def validate[T1 <: HList](header: TypedHeader[F, T1])(implicit
      prep: Prepend[T1, T]): HeaderAppendResult[prep.Out] =
    >>>(header)(prep)
}
