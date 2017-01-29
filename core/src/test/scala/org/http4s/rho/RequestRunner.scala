package org.http4s.rho

import org.http4s._
import org.http4s.HttpService
import scodec.bits.ByteVector

/** Helper for collecting a the body from a `RhoService` */
trait RequestRunner {

  def service: HttpService

  def checkOk(r: Request): String = checkStatus(r)(_ == Status.Ok)

  def checkError(r: Request): String = checkStatus(r)(_ != Status.Ok)

  def checkStatus(r: Request)(isSuccess: Status => Boolean) = {
    val resp = service(r).run
    if (isSuccess(resp.status)) getBody(resp.body)
    else sys.error(s"Invalid response code: ${resp.status}")
  }

  val getBody = RequestRunner.getBody _
}
object RequestRunner {
  def getBody(b: EntityBody): String = {
    new String(b.runLog.run.foldLeft(ByteVector.empty)(_ ++ _).toArray)
  }
}

case class RRunner(val service: HttpService) extends RequestRunner
