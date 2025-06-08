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

package org.http4s.rho.swagger

import org.http4s.Method
import org.http4s.rho.PathBuilder
import org.http4s.rho.PathEmpty
import org.http4s.rho.bits.PathAST
import shapeless.HNil

trait SwaggerSyntax[F[_]] {

  /** Add support for adding documentation to routes using symbolic operators. */
  implicit class StrOps(doc: String) {
    def **(method: Method): PathBuilder[F, HNil] =
      **(new PathBuilder[F, HNil](method, PathEmpty))

    def **[T <: HNil](builder: PathBuilder[F, T]): PathBuilder[F, T] =
      new PathBuilder(builder.method, PathAST.MetaCons(builder.path, RouteDesc(doc)))

    def @@(method: Method): PathBuilder[F, HNil] =
      @@(new PathBuilder[F, HNil](method, PathEmpty))

    def @@[T <: HNil](builder: PathBuilder[F, T]): PathBuilder[F, T] =
      new PathBuilder(builder.method, PathAST.MetaCons(builder.path, RouteTags(List(doc))))
  }

  /** Add support for adding tags before a route using the @@ operator */
  implicit class ListOps(tags: List[String]) {
    def @@(method: Method): PathBuilder[F, HNil] =
      @@(new PathBuilder[F, HNil](method, PathEmpty))

    def @@[T <: HNil](builder: PathBuilder[F, T]): PathBuilder[F, T] =
      new PathBuilder(builder.method, PathAST.MetaCons(builder.path, RouteTags(tags)))
  }
}
