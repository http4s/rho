package org.http4s.rho.swagger

import cats.effect.IO

// TODO: SwaggerSyntax[IO] twice here is weird ...

object ioSyntax
  extends SwaggerSupport[IO](org.http4s.rho.io, new SwaggerSyntax[IO] {})
    with SwaggerSyntax[IO]
