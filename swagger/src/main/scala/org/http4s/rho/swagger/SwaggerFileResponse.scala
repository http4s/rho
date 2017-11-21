package org.http4s.rho.swagger

import cats.effect.IO
import org.http4s.EntityEncoder

final case class SwaggerFileResponse[T](value: T)

object SwaggerFileResponse {
  implicit def entityEncoder[T](implicit encoder: EntityEncoder[IO, T]): EntityEncoder[IO, SwaggerFileResponse[T]] =
    encoder.contramap(_.value)
}

