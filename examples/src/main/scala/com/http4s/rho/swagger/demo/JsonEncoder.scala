package com.http4s.rho.swagger.demo

import java.nio.charset.StandardCharsets

import cats.Applicative
import org.http4s.headers.`Content-Type`
import org.http4s.{Entity, EntityEncoder, MediaType}

object JsonEncoder {
  import fs2.Stream
  import org.json4s._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.write

  trait AutoSerializable extends AnyRef with Product

  private implicit val formats: Formats =
    Serialization.formats(NoTypeHints)

  implicit def autoSerializableEntityEncoder[F[_], A <: AutoSerializable](implicit F: Applicative[F]): EntityEncoder[F, A] =
    EntityEncoder.encodeBy(`Content-Type`(MediaType.`application/json`))(a => F.pure {
      val bytes = write(a).getBytes(StandardCharsets.UTF_8)
      Entity(Stream.emits(bytes), Some(bytes.length))
    })
}
