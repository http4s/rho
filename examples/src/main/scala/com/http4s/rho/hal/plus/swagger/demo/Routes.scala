package com.http4s.rho.hal.plus.swagger.demo

class Routes(businessLayer: BusinessLayer) {

  val dynamicContent = new RestService(businessLayer)

  /**
   * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
   * but its nice to keep it self contained
   */
  val staticContent = StaticContentService.routes

}
