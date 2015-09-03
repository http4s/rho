package com.http4s.rho.swagger.demo

import org.http4s.server.Service
import org.http4s.server.blaze.BlazeBuilder

object Main extends App {
  BlazeBuilder
    .mountService(Service.withFallback(StaticContentService.routes)(MyService.toService))
    .bindLocal(8080)
    .start
    .run
    .awaitShutdown()
}
