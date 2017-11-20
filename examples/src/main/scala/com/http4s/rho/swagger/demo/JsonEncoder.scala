package com.http4s.rho.swagger.demo

import java.nio.charset.StandardCharsets

import cats.effect.IO
import org.http4s.headers.`Content-Type`
import org.http4s.{EntityEncoder, MediaType}
import org.http4s.Entity

// TODO: replace this with http4s json support
object JsonEncoder {
  import org.json4s._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.write

  import fs2.Stream

  trait AutoSerializable extends AnyRef with Product

  private implicit val formats = Serialization.formats(NoTypeHints)

  implicit def jsonWritable[A <: AutoSerializable]: EntityEncoder[IO, A] =
    EntityEncoder.encodeBy[IO, A](`Content-Type`(MediaType.`application/json`))(a => IO.pure {
      val bytes = write(a).getBytes(StandardCharsets.UTF_8)
      Entity(Stream.emits(bytes), Some(bytes.length))
    })
}
