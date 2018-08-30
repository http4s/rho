package org.http4s.rho

import cats.effect.IO
import org.http4s._
import org.http4s.HttpRoutes

/** Helper for collecting a the body from a `RhoService` */
trait RequestRunner {

  def service: HttpRoutes[IO]

  def checkOk(r: Request[IO]): String = checkStatus(r)(_ == Status.Ok)

  def checkError(r: Request[IO]): String = checkStatus(r)(_ != Status.Ok)

  def checkStatus(r: Request[IO])(isSuccess: Status => Boolean) = {
    val resp = service(r).value.unsafeRunSync().getOrElse(Response.notFound)
    if (isSuccess(resp.status)) getBody(resp.body)
    else sys.error(s"Invalid response code: ${resp.status}")
  }

  val getBody = RequestRunner.getBody _
}
object RequestRunner {
  def getBody(b: EntityBody[IO]): String = {
    new String(b.compile.toVector.unsafeRunSync.foldLeft(Array[Byte]())(_ :+ _))
  }
}

case class RRunner(service: HttpRoutes[IO]) extends RequestRunner
