package org.http4s
package rho

import bits.HListToFunc
import bits.HeaderAST.HeaderRule
import bits.PathAST.PathRule

import shapeless.HList

import scala.reflect.runtime.universe.TypeTag


case class RhoAction[T <: HList, F](router: RouteExecutable[T], f: F, hf: HListToFunc[T, F]) {
  final def method: Method = router.method
  final def path: PathRule = router.path
  final def validators: HeaderRule = router.validators
  final def responseEncodings: Seq[MediaType] = hf.encodings
  final def responseType: Option[TypeTag[_]] = hf.typeTag
  final def decoders: Seq[MediaType] = router match {
    case r: CodecRouter[_,_] => r.decoder.consumes
    case _ => Nil
  }
}
