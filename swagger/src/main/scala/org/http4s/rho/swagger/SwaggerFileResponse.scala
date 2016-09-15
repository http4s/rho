package org.http4s.rho.swagger

import org.http4s.EntityEncoder

final case class SwaggerFileResponse[T](value: T)

object SwaggerFileResponse {
  implicit def entityEncoder[T](implicit encoder: EntityEncoder[T]): EntityEncoder[SwaggerFileResponse[T]] =
    encoder.contramap(_.value)
}

