package org.http4s.rho.swagger

import cats.effect.IO
import org.http4s.rho

// TODO: SwaggerSyntax[IO] twice here is weird ...

object ioSyntax
  extends SwaggerSupport[IO](rho.apply[IO], new SwaggerSyntax[IO] {})
    with SwaggerSyntax[IO]
