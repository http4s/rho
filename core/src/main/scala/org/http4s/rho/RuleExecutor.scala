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

import org.http4s.rho.bits.RequestAST, RequestAST._
import org.http4s.rho.bits.{ResultResponse, SuccessResponse}

import shapeless.{HList, HNil}

private[rho] trait RuleExecutor[F[_]] {
  //////////////////////// Stuff for executing the route //////////////////////////////////////

  /** Execute the rule tree */
  def runRequestRules(v: RequestRule[F], req: Request[F]): ResultResponse[F, HList] =
    runRequestRules(req, v, HNil)

  /** Executes the [[RequestRule]] tree pushing the results to `stack` */
  def runRequestRules(req: Request[F], v: RequestRule[F], stack: HList): ResultResponse[F, HList] =
    v match {
      case AndRule(a, b) => runRequestRules(req, a, stack).flatMap(runRequestRules(req, b, _))
      case OrRule(a, b) => runRequestRules(req, a, stack).orElse(runRequestRules(req, b, stack))
      case CaptureRule(reader) => reader(req).map(_ :: stack)
      case MetaRule(r, _) => runRequestRules(req, r, stack)
      case EmptyRule() => SuccessResponse(stack)
      case IgnoreRule(r) => runRequestRules(req, r, stack).map(_ => stack)
      case MapRule(a, f) =>
        runRequestRules(req, a, HNil)
          .map(f.asInstanceOf[HList => HList])
          .map(HList.unsafePrepend(_, stack))
    }
}
