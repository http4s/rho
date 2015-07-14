package org.http4s.rho

import org.http4s._
import scodec.bits.ByteVector

/** Helper for collecting a the body from a `RhoService` */
trait RequestRunner {

  def service: RhoService

  def getBody(b: EntityBody): String = {
    new String(b.runLog.run.foldLeft(ByteVector.empty)(_ ++ _).toArray)
  }

  def checkOk(r: Request): String = checkStatus(r)(_ == Status.Ok)

  def checkError(r: Request): String = checkStatus(r)(_ != Status.Ok)

  def checkStatus(r: Request)(isSuccess: Status => Boolean) = {
    val resp = service.toService(r).run.getOrElse(sys.error(s"Missing path. Uri: ${r.uri}"))
    if (isSuccess(resp.status)) getBody(resp.body)
    else sys.error(s"Invalid response code: ${resp.status}")
  }

}
