package org.http4s
package rho

import bits.RequestRuleAST._

import org.http4s.rho.bits._

import shapeless.{HNil, HList}


trait ExecutableCompiler {

  def parsePath(path: String): List[String] = path.split("/").toList

  //////////////////////// Stuff for executing the route //////////////////////////////////////

  def runRequestRules(rules: RequestRule, req: Request): ResultResponse[HList] =
    runRequestRules(req, rules, HNil)

  /** Walks the validation tree */
  def runRequestRules(req: Request, v: RequestRule, stack: HList): ResultResponse[HList] = {
    import RequestRuleAST.MetaCons
    v match {
      case RequestAnd(a, b) => runRequestRules(req, a, stack).flatMap(runRequestRules(req, b, _))

      case RequestOr(a, b) => runRequestRules(req, a, stack).orElse(runRequestRules(req, b, stack))

      case RequestCapture(reader) => reader.read(req).map(_ :: stack)

      case MetaCons(r, _) => runRequestRules(req, r, stack)

      case EmptyRule => SuccessResponse(stack)
    }
  }
}
