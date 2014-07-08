package org.http4s
package rho

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits.PathAST.PathRule
import org.http4s.rho.bits.QueryAST.QueryRule

import shapeless.HList



trait TypedBuilder[T <: HList] {
  def path: PathRule

  def query: QueryRule

  def validators: HeaderRule
}
