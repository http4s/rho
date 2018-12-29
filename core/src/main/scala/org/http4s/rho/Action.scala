package org.http4s.rho

import org.http4s.rho.bits.ResultInfo
import org.http4s.{AuthedRequest, MediaType, Request, Response}
import shapeless.HList

/** Encapsulation of metadata and a result generator
  *
  * @param resultInfo Information about the status and type the Action will produce.
  * @param responseEncodings Encodings that the response supports.
  * @param act Function of [[R]] and the [[HList]] to a `F[Response]`
  * @tparam T The type of [[HList]] required to execute the [[AbstractAction]].
  * @tparam F The effect type
  * @tparam R The type of request
  */
case class AbstractAction[F[_], R[_[_]], T <: HList](resultInfo: Set[ResultInfo],
                                    responseEncodings: Set[MediaType],
                                    act: (R[F], T) => F[Response[F]])

object Action {
  def apply[F[_], T <: HList](resultInfo: Set[ResultInfo],
                              responseEncodings: Set[MediaType],
                              act: (Request[F], T) => F[Response[F]]): Action[F, T] =
    AbstractAction(resultInfo, responseEncodings, act)
}

object AuthedAction {
  def apply[F[_], T <: HList, U](resultInfo: Set[ResultInfo],
                              responseEncodings: Set[MediaType],
                              act: (AuthedRequest[F, U], T) => F[Response[F]]): AuthedAction[F, T, U] =
    AbstractAction[F, AuthedRequest[?[_], U], T](resultInfo, responseEncodings, act)
}