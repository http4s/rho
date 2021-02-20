package org.http4s.rho.swagger

import org.http4s.EntityEncoder

final case class SwaggerFileResponse[T](value: T)

object SwaggerFileResponse {
  implicit def entityEncoder[F[_], T](implicit
      encoder: EntityEncoder[F, T]): EntityEncoder[F, SwaggerFileResponse[T]] =
    encoder.contramap(_.value)
}
