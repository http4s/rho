package org.http4s.rho.swagger

import cats.effect.IO

object syntax {
  object io extends SwaggerSupport[IO](org.http4s.rho.io)
}
