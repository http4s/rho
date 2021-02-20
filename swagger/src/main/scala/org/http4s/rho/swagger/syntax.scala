package org.http4s.rho.swagger

import cats.effect.IO

object syntax {
  val io = SwaggerSupport.apply[IO]
}
