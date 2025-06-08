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

package org.http4s
package rho

import cats.effect.IO
import fs2.Stream
import munit.CatsEffectSuite

import scala.collection.compat.immutable.ArraySeq

class CodecRouterSuite extends CatsEffectSuite {
  private def bodyAndStatus(resp: Response[IO]): IO[(String, Status)] = {
    val rbody = resp.body.compile.toVector.map(_.foldLeft(Array[Byte]())(_ :+ _)).map(new String(_))
    rbody.map(_ -> resp.status)
  }

  val routes: HttpRoutes[IO] = new RhoRoutes[IO] {
    (POST / "foo" decoding (EntityDecoder.text[IO])) |>> { s: String => Ok(s"Received: $s") }
    (POST / "form" decoding (UrlForm.entityDecoder[IO])) |>> { _: UrlForm => Ok("success") }
  }.toRoutes()

  test("A CodecRouter in a RhoRoutes should decode a valid body") {
    val b = Stream.emits(ArraySeq.unsafeWrapArray("hello".getBytes))
    val h = Headers(headers.`Content-Type`(MediaType.text.plain))
    val req = Request[IO](Method.POST, uri"/foo", headers = h, body = b)

    for {
      result <- routes(req).value.map(_.getOrElse(Response.notFound))
      _ <- assertIO(bodyAndStatus(result), "Received: hello" -> Status.Ok)
    } yield ()
  }

  test("A CodecRouter in a RhoRoutes should fail on invalid body") {
    val b = Stream.emits(ArraySeq.unsafeWrapArray("hello =".getBytes))
    val h = Headers(headers.`Content-Type`(MediaType.application.`x-www-form-urlencoded`))
    val req =
      Request[IO](Method.POST, uri"/form", headers = h, body = b)

    assertIO(routes(req).value.map(_.map(_.status)), Some(Status.BadRequest))
  }
}
