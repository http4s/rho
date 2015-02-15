package org.http4s.rho

import org.http4s.{Response, Status}

/**
 * Created by bryce on 2/15/15.
 */
private[rho] object SyncRespBuilder {
  def apply(status: Status)(body: String): Response = Response(status = status).withBody(body).run
  def badRequest(msg: String): Response = apply(Status.BadRequest)(msg)
}
