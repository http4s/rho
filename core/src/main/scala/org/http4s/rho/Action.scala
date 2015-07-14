package org.http4s.rho

import org.http4s.{Response, Request, MediaType}
import org.http4s.rho.bits.{ResultInfo, HListToFunc}
import shapeless.HList

import scalaz.concurrent.Task

/**
 * Created on 7/9/15.
 */
case class Action[H <: HList](resultInfo: Set[ResultInfo],
                               responseEncodings: Set[MediaType],
                               act: (Request, H) => Task[Response]
                              )

object Action {
//  def apply[H <: HList, F](f: F)(implicit ev: HListToFunc[H, F]): Action[H] = ev.toAction(f)
}
