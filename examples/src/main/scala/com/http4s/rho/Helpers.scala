package com.http4s.rho

import org.http4s.server.HttpService
import org.http4s.{Request, Status}

import scalaz.Kleisli
import scalaz.concurrent.Task

 object Helpers {

   // TODO: this should be in http4s proper
  /** Helper so that a HttpServices can be chained */
  implicit class OrElseSyntax(val service: HttpService) extends AnyVal {
    def orElse(service2: HttpService): HttpService = Kleisli { req: Request =>
      service(req).flatMap {
        case resp if resp.status == Status.NotFound => service2(req)
        case resp => Task.now(resp)
      }
    }
  }

}
