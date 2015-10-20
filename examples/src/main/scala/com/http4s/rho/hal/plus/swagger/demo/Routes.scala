package com.http4s.rho.hal.plus.swagger.demo

import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.server.HttpService

class Routes(businessLayer: BusinessLayer) {

  val dynamicContent: HttpService = new RestService(businessLayer).toService(SwaggerSupport())

  /**
   * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
   * but its nice to keep it self contained
   */
  val staticContent = StaticContentService.routes

}
