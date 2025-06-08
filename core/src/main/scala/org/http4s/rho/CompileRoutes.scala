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

import scala.collection.immutable.Seq
import cats._
import shapeless.HList
import org.http4s.rho.RhoRoute.Tpe
import org.http4s.rho.bits.PathTree

/** Transforms a [[RhoRoute]] into an `RouteType`.
  *
  * This can be a stateful operation, storing the action for later execution or any other type of
  * compilation phase.
  */
trait CompileRoutes[F[_], RouteType] {

  /** Transform the [[RhoRoute]] into a `RouteType` possibly mutating this compilers state.
    *
    * @param route
    *   [[RhoRoute]] to compile.
    * @tparam T
    *   `HList` representation of the result of the route
    * @return
    *   The result of the compilation process.
    */
  def compile[T <: HList](route: RhoRoute[F, T]): RouteType
}

object CompileRoutes {

  /** [[CompileRoutes]] that simply returns its argument */
  def identityCompiler[F[_]]: CompileRoutes[F, Tpe[F]] = new CompileRoutes[F, RhoRoute.Tpe[F]] {
    def compile[T <: HList](route: RhoRoute[F, T]): RhoRoute[F, T] = route
  }

  /** Importable implicit identity compiler */
  object Implicit {
    implicit def compiler[F[_]]: CompileRoutes[F, RhoRoute.Tpe[F]] = identityCompiler[F]
  }

  /** Convert the `Seq` of [[RhoRoute]]'s into a `HttpRoutes`
    *
    * @param routes
    *   `Seq` of routes to bundle into a service.
    * @return
    *   An `HttpRoutes`
    */
  def foldRoutes[F[_]: Monad](routes: Seq[RhoRoute.Tpe[F]]): HttpRoutes[F] = {
    val tree = routes.foldLeft(PathTree[F]())((t, r) => t.appendRoute(r))
    HttpRoutes((req: Request[F]) => tree.getResult(req).toResponse)
  }
}
