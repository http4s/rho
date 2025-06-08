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

import org.http4s.rho.bits.PathAST.PathRule
import org.http4s.rho.bits.RequestAST.RequestRule
import org.http4s.rho.bits.UriConverter

import shapeless.HList

/** A typed shell which represents the requirements of the route
  * @tparam T
  *   The `HList` representation of the values to be extracted from the `Request`.
  */
trait TypedBuilder[F[_], T <: HList] extends UriConvertible[F] {

  /** Untyped AST representation of the path to operate on */
  val path: PathRule

  /** Untyped AST describing the extraction of headers and the query from the `Request` */
  val rules: RequestRule[F]

  private def uriTemplate =
    for {
      p <- UriConverter.createPath(path)
      q <- UriConverter.createQuery(rules)
    } yield UriTemplate(path = p, query = q)

  final override def asUriTemplate(request: Request[F]) =
    UriConvertible.respectPathInfo(uriTemplate, request)
}
