//package org.http4s
//package rho.bits
//
//import org.http4s.Header.`Content-Type`
//import org.http4s.Writable.Entity
//
//import scala.reflect.runtime.universe.TypeTag
//import scalaz.concurrent.Task
//
//
//trait ObjToResponse[O] {
//  def apply(o: O): Task[Response]
//  def mediaTypes: Set[MediaType]
//  def typeTag: Option[TypeTag[O]]
//}
//
//object ObjToResponse {
//  implicit def taskResponse: ObjToResponse[Task[Response]] = new ObjToResponse[Task[Response]] {
//
//    override def mediaTypes = Set.empty
//
//    override def apply(o: Task[Response]): Task[Response] = o
//
//    override def typeTag: Option[TypeTag[Task[Response]]] = None
//  }
//
//  implicit def writableResponse[O](implicit w: Writable[O], m: TypeTag[O]): ObjToResponse[O] = new ObjToResponse[O] {
//
//    override def typeTag: Option[TypeTag[O]] = Some(m)
//
//    override val mediaTypes: Set[MediaType] = {
//      w.headers.get(`Content-Type`).map(c => Set(c.mediaType)).getOrElse(Set.empty)
//    }
//
//    private val mediaHeader = w.headers.get(`Content-Type`) match {
//      case Some(c) => c::Nil
//      case _       =>    Nil
//    }
//
//    override def apply(o: O): Task[Response] = w.toEntity(o).map {
//      case Entity(body, Some(i)) => Response(Status.Ok,
//        headers = Headers(Header.`Content-Length`(i)::mediaHeader),
//        body = body)
//
//      case Entity(body, None) => Response(Status.Ok, headers = Headers(mediaHeader), body = body)
//    }
//  }
//}