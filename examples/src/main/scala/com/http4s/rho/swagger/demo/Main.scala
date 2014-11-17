package com.http4s.rho.swagger.demo

import org.http4s.{Request, Response, Header}
import org.http4s.Header.{ `Access-Control-Allow-Origin` }
import org.http4s.server.Service
import org.http4s.server.blaze.BlazeServer
import JsonWritable.AutoSerializable

import scalaz.concurrent.Task

object Main extends App {
  val builder = BlazeServer.newBuilder
  val myService = MyService.toService.map { r =>
    r.putHeaders(Header(`Access-Control-Allow-Origin`.name.toString, "*"))
  }
  builder
    .mountService(StaticContentService.routes)
    .mountService(myService)
    .withPort(8080)
    .build
    .run()
}
