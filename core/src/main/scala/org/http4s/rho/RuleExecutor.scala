package org.http4s
package rho

import org.http4s.rho.bits.RequestAST, RequestAST._
import org.http4s.rho.bits.{ ResultResponse, SuccessResponse }

import shapeless.{ HNil, HList }


object RuleExecutor {
  //////////////////////// Stuff for executing the route //////////////////////////////////////

  /** Execute the rule tree */
  def runRequestRules[F[_]](v: RequestRule[F], req: Request[F]): ResultResponse[F, HList] =
    runRequestRules(req, v, HNil)


  /** Executes the [[RequestRule]] tree pushing the results to `stack` */
  def runRequestRules[F[_]](req: Request[F], v: RequestRule[F], stack: HList): ResultResponse[F, HList] = v match {
    case AndRule(a, b) => runRequestRules(req, a, stack).flatMap(runRequestRules(req, b, _))
    case OrRule(a, b) => runRequestRules(req, a, stack).orElse(runRequestRules(req, b, stack))
    case CaptureRule(reader) => reader(req).map(_::stack)
    case MetaRule(r, _) => runRequestRules(req, r, stack)
    case EmptyRule() => SuccessResponse(stack)
    case IgnoreRule(r) => runRequestRules(req, r, stack).map(_ => stack)
    case MapRule(a, f) =>
      runRequestRules(req, a, HNil).map(f.asInstanceOf[HList => HList]).map(HList.unsafePrepend(_, stack))
  }
}
