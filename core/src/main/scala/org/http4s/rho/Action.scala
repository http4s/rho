package org.http4s.rho

import org.http4s.{Response, Request, MediaType}
import org.http4s.rho.bits.ResultInfo
import shapeless.HList

import scalaz.concurrent.Task

final case class Action[T <: HList](
                   resultInfo: Set[ResultInfo],
                   responseEncodings: Set[MediaType],
                   act: (Request, T) => Task[Response]
                 )
{ type Tpe = T }

