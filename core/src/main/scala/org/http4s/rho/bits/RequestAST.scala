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

/** Rules for extracting non-path related data from the `Request`
  *
  * This AST is subject to change.
  */
object RequestAST {

  /** Base trait of the AST */
  sealed trait RequestRule[F[_]]

  /** Captures data from the `Request` */
  case class CaptureRule[F[_], T](reader: Request[F] => ResultResponse[F, T]) extends RequestRule[F]

  /** Transform data from the `Request` */
  case class MapRule[F[_], T <: HList, R <: HList](rule: RequestRule[F], f: T => R)
      extends RequestRule[F]

  /** Ignore any results obtained form the enclosed rules */
  case class IgnoreRule[F[_]](rule: RequestRule[F]) extends RequestRule[F]

  /** Append the result as `b:::a:::HNil` */
  case class AndRule[F[_]](fst: RequestRule[F], snd: RequestRule[F]) extends RequestRule[F]

  /** Attempt rule `fst` and attempt rule `alt` if `fst` fails */
  case class OrRule[F[_]](fst: RequestRule[F], alt: RequestRule[F]) extends RequestRule[F]

  /** Append meta data to the tree */
  case class MetaRule[F[_]](rule: RequestRule[F], meta: Metadata) extends RequestRule[F]

  /** Empty rule */
  case class EmptyRule[F[_]]() extends RequestRule[F]
}
