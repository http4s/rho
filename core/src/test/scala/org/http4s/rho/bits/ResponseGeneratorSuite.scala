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

package org.http4s.rho.bits

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.headers.{Location, `Content-Length`, `Content-Type`, `Transfer-Encoding`}
import org.http4s.rho.io._

class ResponseGeneratorSuite extends CatsEffectSuite {
  test("A ResponseGenerator should build a response with a body") {
    for {
      resp <- Ok("Foo").map(_.resp)
      strData <- resp.body.compile.toVector.map(_.foldLeft(Array[Byte]())(_ :+ _))

      _ = assertEquals(new String(strData), "Foo")

      _ = assertEquals(
        resp.headers.get[`Content-Length`],
        Some(
          `Content-Length`.unsafeFromLong("Foo".getBytes.length)
        )
      )

      _ = assertEquals(
        resp.headers.headers
          .filterNot(_.name == `Content-Length`.name),
        EntityEncoder.stringEncoder.headers.headers
      )
    } yield ()
  }

  test("A ResponseGenerator should build a response without a body") {
    for {
      resp <- SwitchingProtocols.apply.map(_.resp)
      _ <- assertIO(resp.body.compile.toVector.map(_.length), 0)
      _ = assertEquals(resp.status, Status.SwitchingProtocols)
      _ = assertEquals(resp.headers.get[`Content-Length`], None)
      _ = assertEquals(resp.headers.get[`Transfer-Encoding`], None)
    } yield ()
  }

  test("A ResponseGenerator should build a redirect response") {
    val location = Uri.fromString("/foo").getOrElse(sys.error("Fail."))

    for {
      resp <- MovedPermanently(location).map(_.resp)
      _ <- assertIO(resp.body.compile.toVector.map(_.length), 0)
      _ = assertEquals(resp.status, Status.MovedPermanently)
      _ = assertEquals(resp.headers.get[`Content-Length`], None)
      _ = assertEquals(resp.headers.get[`Transfer-Encoding`], None)
      _ = assertEquals(resp.headers.get[Location], Some(Location(location)))
    } yield ()
  }

  test("A ResponseGenerator should build a redirect response with a body") {
    val testBody = "Moved!!!"
    val location = Uri.fromString("/foo").getOrElse(sys.error("Fail."))

    for {
      resp <- MovedPermanently(location, testBody).map(_.resp)

      _ <- assertIO(
        EntityDecoder[IO, String].decode(resp, false).value,
        Right(testBody)
      )

      _ = assertEquals(resp.status, Status.MovedPermanently)

      _ = assertEquals(
        resp.headers.get[`Content-Length`],
        Some(
          `Content-Length`.unsafeFromLong(testBody.length)
        )
      )

      _ = assertEquals(resp.headers.get[`Transfer-Encoding`], None)
      _ = assertEquals(resp.headers.get[Location], Some(Location(location)))
    } yield ()
  }

  test("A ResponseGenerator should explicitly added headers have priority") {
    implicit val w: EntityEncoder[IO, String] =
      EntityEncoder.encodeBy[IO, String](`Content-Type`(MediaType.text.html))(
        EntityEncoder.stringEncoder[IO].toEntity(_)
      )

    assertIO(
      Ok("some content", Headers(`Content-Type`(MediaType.application.json)))
        .map(_.resp.headers.get[`Content-Type`].get),
      `Content-Type`(MediaType.application.json)
    )
  }
}
