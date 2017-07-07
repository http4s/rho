package com.http4s.rho.swagger.demo

import java.nio.charset.StandardCharsets

import org.http4s.headers.`Content-Type`
import org.http4s.{MediaType, EntityEncoder}
import org.http4s.Entity

// TODO: replace this with http4s json support
object JsonEncoder {
  import org.json4s._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.write

  import fs2.{Task, Stream}

  trait AutoSerializable extends AnyRef with Product

  private implicit val formats = Serialization.formats(NoTypeHints)

  implicit def jsonWritable[A <: AutoSerializable]: EntityEncoder[A] =
    EntityEncoder.encodeBy(`Content-Type`(MediaType.`application/json`))(a => Task.now {
      val bytes = write(a).getBytes(StandardCharsets.UTF_8)
      Entity(Stream.emits(bytes), Some(bytes.length))
    })
}
