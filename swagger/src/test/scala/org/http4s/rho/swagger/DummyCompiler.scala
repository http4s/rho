package org.http4s.rho.swagger

import org.http4s.rho.{RhoAction, CompileService}
import shapeless.HList


object DummyCompiler {

  implicit def compilerInstance[F] = new CompileService[F, RhoAction[_ <: HList, F]] {
    override def compile(action: RhoAction[_ <: HList, F]) = action
  }

}
