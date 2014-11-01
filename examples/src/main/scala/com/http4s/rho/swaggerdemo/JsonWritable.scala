package com.http4s.rho.swaggerdemo

import java.nio.charset.StandardCharsets

import org.http4s.Header.`Content-Type`
import org.http4s.Headers
import org.http4s.Writable.Entity

// TODO: replace this with http4s json support
object JsonWritable {
  import org.http4s.Writable
  import org.json4s._
  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.write
  import scodec.bits.ByteVector

import scalaz.concurrent.Task
  import scalaz.stream.Process.emit

  trait AutoSerializable extends AnyRef with Product

  private implicit val formats = Serialization.formats(NoTypeHints)

  implicit def jsonWritable[A <: AutoSerializable]: Writable[A] =
    Writable[A](a => Task.now {
      val bytes = write(a).getBytes(StandardCharsets.UTF_8)
      Entity(emit(ByteVector.view(bytes)), Some(bytes.length))
    }, Headers.empty).withContentType(`Content-Type`.`application/json`)
}
