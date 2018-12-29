package org.http4s.rho

package object bits {
  implicit def requestLikeOps[R[_[_]], F[_]](r: R[F])(implicit rl: RequestLike[R]): RequestLikeOps[R, F] = RequestLike.ops[R, F](r)
}
