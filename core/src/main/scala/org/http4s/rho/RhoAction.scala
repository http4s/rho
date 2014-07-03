package org.http4s
package rho

import bits.HListToFunc
import bits.HeaderAST.HeaderRule
import bits.PathAST.PathRule

import shapeless.HList


case class RhoAction[T <: HList, F, O](private[rho] val router: RouteExecutable[T],
                                       private[rho] val f: F,
                                       private[rho] val hf: HListToFunc[T, O, F]) {
  final def method: Method = router.method
  final def path: PathRule[_ <: HList] = router.path
  final def validators: HeaderRule[_ <: HList] = router.validators
  final def responseEncodings: Seq[MediaType] = hf.encodings
  final def responseType: Option[Manifest[O]] = hf.manifest
  final def decoders: Seq[MediaType] = router match {
    case r: CodecRouter[_,_] => r.decoder.consumes
    case _ => Nil
  }
}
