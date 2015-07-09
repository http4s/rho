package org.http4s.rho.swagger

import org.http4s.rho.{RhoAction, CompileService}
import shapeless.HList


object DummyCompiler {
  implicit def compilerInstance = new CompileService[RhoAction[_ <: HList]] {
    override def compile(action: RhoAction[_ <: HList]) = action
  }
}
