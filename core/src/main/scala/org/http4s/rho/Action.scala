package org.http4s.rho

import org.http4s.{Response, Request, MediaType}
import org.http4s.rho.bits.ResultInfo
import shapeless.HList

import fs2.Task

/** Encapsulation of metadata and a result generator
  *
  * @param resultInfo Information about the status and type the Action will produce.
  * @param responseEncodings Encodings that the response supports.
  * @param act Function of `Request` and the `HList` to a `Task[Response]`
  * @tparam T The type of `HList` required to execute the [[Action]].
  */
case class Action[T <: HList](resultInfo: Set[ResultInfo],
                              responseEncodings: Set[MediaType],
                              act: (Request, T) => Task[Response])

