package org.http4s
package rho

import bits.HeaderAST.HeaderRule
import bits.PathAST.PathRule
import org.http4s.rho.bits.QueryAST.QueryRule
import org.http4s.rho.bits.ResultInfo

import shapeless.HList


final case class RhoAction[T <: HList](router: RoutingEntity[T], action: AAction[T]) {
  def method: Method = router.method
  def path: PathRule = router.path
  def query: QueryRule = router.query
  def headers: HeaderRule = router.headers
  def responseEncodings: Set[MediaType] = action.responseEncodings
  def resultInfo: Set[ResultInfo] = action.resultInfo
  def validMedia: Set[MediaRange] = router match {
    case r: CodecRouter[_,_] => r.decoder.consumes
    case _ => Set.empty
  }
}
