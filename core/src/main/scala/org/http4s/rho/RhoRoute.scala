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

import org.http4s.rho.bits.PathAST.{PathRule, TypedPath}
import org.http4s.rho.bits.RequestAST.RequestRule
import org.http4s.rho.bits.ResultInfo

import shapeless.{HList, HNil}

/** A type to bundle everything needed to define a route */
final case class RhoRoute[F[_], T <: HList](router: RoutingEntity[F, T], action: Action[F, T])
    extends RoutePrependable[F, RhoRoute[F, T]] {

  /** Execute the [[RhoRoute]]
    *
    * @param req
    *   The `Request` to be served.
    * @param hlist
    *   Parameters obtained by executing the rules.
    * @return
    *   A `Response` to the `Request`.
    */
  def apply(req: Request[F], hlist: T): F[Response[F]] = action.act(req, hlist)

  /** Prefix the [[RhoRoute]] with non-capturing path rules
    *
    * @param prefix
    *   non-capturing prefix to prepend
    * @return
    *   builder with the prefix prepended to the path rules
    */
  override def /:(prefix: TypedPath[F, HNil]): RhoRoute[F, T] =
    copy(router = prefix /: router)

  def method: Method = router.method
  def path: PathRule = router.path
  def rules: RequestRule[F] = router.rules
  def responseEncodings: Set[MediaType] = action.responseEncodings
  def resultInfo: Set[ResultInfo] = action.resultInfo
  def validMedia: Set[MediaRange] = router match {
    case r: CodecRouter[F, _, _] => r.decoder.consumes
    case _ => Set.empty
  }
}

object RhoRoute {

  /** Existentially typed [[RhoRoute]] useful when the parameters are not needed */
  type Tpe[F[_]] = RhoRoute[F, _ <: HList]
}
