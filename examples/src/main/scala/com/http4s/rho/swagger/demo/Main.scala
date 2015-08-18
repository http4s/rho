package com.http4s.rho.swagger.demo

import org.http4s.server.blaze.BlazeBuilder
import com.http4s.rho.Helpers._

object Main extends App {
  BlazeBuilder
    .mountService(StaticContentService.routes orElse MyService.toService)
    .bindLocal(8080)
    .start
    .run
    .awaitShutdown()
}
