package com.http4s.rho.swagger.demo

import org.http4s.server.blaze.BlazeBuilder

object Main extends App {
  BlazeBuilder
    .mountService(StaticContentService.routes)
    .mountService(MyService.toService)
    .bindLocal(8080)
    .start
    .run
}
