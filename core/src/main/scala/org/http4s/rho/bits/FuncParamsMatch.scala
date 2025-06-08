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

import scala.annotation.implicitNotFound
import scala.annotation.nowarn

@implicitNotFound("""Could not bind route to action;
  Expecting a function with parameter types matching:
         (${T}) 
  or     (Request[F] :: ${T})
  found: ${FU}""")
trait FuncParamsMatch[F[_], T <: HList, -FU]

trait FuncParamsMatchers[F[_]] {

  import shapeless._
  import shapeless.ops.function._
  import shapeless.ops.hlist._

  implicit def constantParamsMatch[R]: FuncParamsMatch[F, HNil, R] =
    new FuncParamsMatch[F, HNil, R] {}

  @nowarn("cat=unused")
  implicit def functionParamsMatch[T <: HList, TR <: HList, FU, R](implicit
      fp: FnToProduct.Aux[FU, TR => R],
      rev: Reverse.Aux[T, TR]): FuncParamsMatch[F, T, FU] = new FuncParamsMatch[F, T, FU] {}

  @nowarn("cat=unused")
  implicit def functionWithRequestParamsMatch[T <: HList, TR <: HList, FU, R](implicit
      fp: FnToProduct.Aux[FU, Request[F] :: TR => R],
      rev: Reverse.Aux[T, TR]): FuncParamsMatch[F, T, FU] = new FuncParamsMatch[F, T, FU] {}
}
