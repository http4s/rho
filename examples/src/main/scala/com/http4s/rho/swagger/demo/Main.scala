package com.http4s.rho.swagger.demo

import org.http4s.Header
import org.http4s.Header.`Access-Control-Allow-Origin`
import org.http4s.server.blaze.BlazeBuilder

object Main extends App {
  val myService = MyService.toService.map { r =>
    r.putHeaders(Header(`Access-Control-Allow-Origin`.name.toString, "*"))
  }
  BlazeBuilder
    .mountService(StaticContentService.routes)
    .mountService(myService)
    .bindLocal(8080)
    .start
    .run
}
