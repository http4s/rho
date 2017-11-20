package org.http4s.rho

import org.http4s._
import org.http4s.HttpService
import scodec.bits.ByteVector

/** Helper for collecting a the body from a `RhoService` */
trait RequestRunner {

  def service: HttpService

  def checkOk(r: Request[IO]): String = checkStatus(r)(_ == Status.Ok)

  def checkError(r: Request[IO]): String = checkStatus(r)(_ != Status.Ok)

  def checkStatus(r: Request[IO])(isSuccess: Status => Boolean) = {
    val resp = service(r).unsafeRun.orNotFound
    if (isSuccess(resp.status)) getBody(resp.body)
    else sys.error(s"Invalid response code: ${resp.status}")
  }

  val getBody = RequestRunner.getBody _
}
object RequestRunner {
  def getBody(b: EntityBody): String = {
    new String(b.runLog.unsafeRun.foldLeft(ByteVector.empty)(_ :+ _).toArray)
  }
}

case class RRunner(val service: HttpService) extends RequestRunner
