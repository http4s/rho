/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.rho

import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s._

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

final case class RRunner(httpRoutes: HttpRoutes[IO]) extends RequestRunner
