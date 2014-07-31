package org.http4s
package rho

import bits.HListToFunc
import bits.HeaderAST.HeaderRule
import bits.PathAST.PathRule
import org.http4s.rho.bits.QueryAST.QueryRule

import shapeless.HList

import scala.reflect.runtime.universe.TypeTag


case class RhoAction[T <: HList, F](router: RouteExecutable[T], f: F, hf: HListToFunc[T, F]) {
  final def method: Method = router.method
  final def path: PathRule = router.path
  final def query: QueryRule = router.query
  final def headers: HeaderRule = router.validators
  final def responseEncodings: Set[MediaType] = hf.encodings
  final def responseType: Option[TypeTag[_]] = hf.typeTag
  final def decoders: Set[MediaType] = router match {
    case r: CodecRouter[_,_] => r.decoder.consumes
    case _ => Set.empty
  }
}
