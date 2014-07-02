package org.http4s
package rho.bits

import scalaz.concurrent.Task


trait ObjToResponse[O] {
  def apply(o: O): Task[Response]
  def mediaTypes: Seq[MediaType]
  def manifest: Option[Manifest[O]]
}

object ObjToResponse {
  implicit val taskResponse = new ObjToResponse[Task[Response]] {
    override def mediaTypes = Nil
    override def apply(o: Task[Response]): Task[Response] = o

    override def manifest: Option[Manifest[Task[Response]]] = None
  }

  implicit def writableResponse[O](implicit w: Writable[O], m: Manifest[O]) = new ObjToResponse[O] {

    override def manifest: Some[Manifest[O]] = Some(m)

    override def mediaTypes: Seq[MediaType] = w.contentType.mediaType::Nil

    override def apply(o: O): Task[Response] = w.toBody(o).map {
      case (body, Some(i)) => Response(Status.Ok, headers = Headers(Header.`Content-Length`(i)), body = body)
      case (body, None) => Response(Status.Ok, headers = Headers.empty, body = body)
    }
  }
}