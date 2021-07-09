package org.http4s.rho

import cats.effect.IO
import org.http4s._
import org.http4s.HttpRoutes

/** Helper for collecting a the body from a `RhoRoutes` */
trait RequestRunner {
  def httpRoutes: HttpRoutes[IO]

  def checkOk(req: Request[IO]): IO[String] = checkStatus(req)(_ == Status.Ok)

  def checkError(req: Request[IO]): IO[String] = checkStatus(req)(_ != Status.Ok)

  def checkStatus(req: Request[IO])(isSuccess: Status => Boolean): IO[String] =
    for {
      resp <- httpRoutes(req).value.map(_.getOrElse(Response.notFound))
      result <-
        if (isSuccess(resp.status)) getBody(resp.body)
        else IO.raiseError[String](new Throwable(s"Invalid response code: ${resp.status}"))
    } yield result

  val getBody: EntityBody[IO] => IO[String] = RequestRunner.getBody
}

object RequestRunner {
  def getBody(b: EntityBody[IO]): IO[String] =
    b.compile.toVector.map(_.foldLeft(Array[Byte]())(_ :+ _)).map(new String(_))
}

case class RRunner(httpRoutes: HttpRoutes[IO]) extends RequestRunner
