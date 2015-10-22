package org.http4s.rho

import org.http4s.rho.bits.PathAST.TypedPath
import shapeless.HNil

/** Types that extends this trait can be prepended with noncapturing paths */
trait RoutePrependable[T <: RoutePrependable[T]] {
  def /:(prefix: TypedPath[HNil]): T
}
