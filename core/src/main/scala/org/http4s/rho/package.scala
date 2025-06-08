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

import org.http4s.rho.PathBuilder
import org.http4s.rho.PathEmpty
import org.http4s.rho.ResultSyntaxInstances
import org.http4s.rho.RhoDslHeaderExtractors
import org.http4s.rho.RhoDslPathExtractors
import org.http4s.rho.RhoDslQueryParamExtractors
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits._
import shapeless.HList
import shapeless.HNil

import scala.collection.immutable.Seq

package object rho extends org.http4s.syntax.AllSyntax {
  type RhoMiddleware[F[_]] = Seq[RhoRoute[F, _ <: HList]] => Seq[RhoRoute[F, _ <: HList]]

  val PathEmpty: PathRule = PathMatch("")
}

trait RhoDsl[F[_]]
    extends RhoDslQueryParamExtractors[F]
    with RhoDslPathExtractors[F]
    with RhoDslHeaderExtractors[F]
    with ResultSyntaxInstances[F]
    with QueryParsers[F]
    with MatchersHListToFunc[F]
    with FuncParamsMatchers[F]
    with ResponseGeneratorInstances[F]
    with FailureResponseOps[F]
    with ResultMatchers[F] {

  implicit def method(m: Method): PathBuilder[F, HNil] = new PathBuilder(m, PathEmpty)

  /** Helper to be able to define a path with one level only.
    * {{{
    * val hello = Root / "hello"
    * }}}
    */
  def root: TypedPath[F, HNil] = TypedPath(PathEmpty)

  def * : CaptureTail.type = CaptureTail
}

object RhoDsl {
  def apply[F[_]]: RhoDsl[F] = new RhoDsl[F] {}
}
