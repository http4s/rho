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

package org.http4s.rho

import org.http4s.MediaType
import org.http4s.Request
import org.http4s.Response
import org.http4s.rho.bits.ResultInfo
import shapeless.HList

/** Encapsulation of metadata and a result generator
  *
  * @param resultInfo
  *   Information about the status and type the Action will produce.
  * @param responseEncodings
  *   Encodings that the response supports.
  * @param act
  *   Function of `Request` and the `HList` to a `Task[Response]`
  * @tparam T
  *   The type of `HList` required to execute the [[Action]].
  */
final case class Action[F[_], T <: HList](
    resultInfo: Set[ResultInfo],
    responseEncodings: Set[MediaType],
    act: (Request[F], T) => F[Response[F]])
