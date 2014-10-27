package org.http4s
package rho
package bits

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits.QueryAST.QueryRule
import shapeless.HList


// Note that these methods don't preserve the full type structure of the HList and will need to be cast at some point
private[bits] object TempTools extends ExecutableCompiler {

  override def runValidation(req: Request, v: HeaderRule, stack: HList): ParserResult[HList] =
    super.runValidation(req, v, stack)

  override def runQuery(req: Request, v: QueryRule, stack: HList): ParserResult[HList] =
    super.runQuery(req, v, stack)
}