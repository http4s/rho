package org.http4s.rho

import cats.effect.IO
import org.http4s._
import org.http4s.HttpRoutes

/** Helper for collecting a the body from a `RhoRoutes` */
trait RequestRunner {

  def httpRoutes: HttpRoutes[IO]

  def checkOk(req: Request[IO]): String = checkStatus(req)(_ == Status.Ok)

  def checkError(req: Request[IO]): String = checkStatus(req)(_ != Status.Ok)

  def checkStatus(req: Request[IO])(isSuccess: Status => Boolean): String = {
    val resp = httpRoutes(req).value.unsafeRunSync().getOrElse(Response.notFound)
    if (isSuccess(resp.status)) getBody(resp.body)
    else sys.error(s"Invalid response code: ${resp.status}")
  }

  val getBody = RequestRunner.getBody _
}

object RequestRunner {
  def getBody(b: EntityBody[IO]): String = {
    new String(b.compile.toVector.unsafeRunSync().foldLeft(Array[Byte]())(_ :+ _))
  }
}

case class RRunner(httpRoutes: HttpRoutes[IO]) extends RequestRunner
