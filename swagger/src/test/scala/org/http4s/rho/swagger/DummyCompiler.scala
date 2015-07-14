package org.http4s.rho.swagger

import org.http4s.rho.{RhoRoute, CompileService}
import shapeless.HList


object DummyCompiler {
  implicit def compilerInstance = new CompileService[RhoRoute[_ <: HList]] {
    override def compile(route: RhoRoute[_ <: HList]) = route
  }
}
