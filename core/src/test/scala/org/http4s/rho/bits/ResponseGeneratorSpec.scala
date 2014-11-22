//package org.http4s.rho.bits
//
//import org.http4s.Header.{`Content-Type`, Location, `Transfer-Encoding`, `Content-Length`}
//import org.http4s._
//import org.http4s.rho.{EmptyResult, Result}
//import org.specs2.mutable.Specification
//
//import scalaz.concurrent.Task
//
//
//class ResponseGeneratorSpec extends Specification {
//  import ResponseGeneratorInstances._
//
//  "ResponseGenerator" should {
//    "Build a response with a body" in {
//      val result: Result[Status.Ok.type , String] = Ok("Foo").run
//      val resp = result.resp
//
//      val str = new String(resp.body.runLog.run.reduce(_ ++ _).toArray)
//      str must_== "Foo"
//
//      resp.headers.get(`Content-Length`) must_== Some(`Content-Length`("Foo".getBytes.length))
//      resp.headers.filterNot(_.is(`Content-Length`)).toList must_== Writable.stringWritable.headers.toList
//    }
//
//    "Build a response without a body" in {
//      val result: Result[Status.Ok.type, EmptyResult] = SwitchingProtocols().run
//      val resp = result.resp
//
//      resp.body.runLog.run.length must_== 0
//      resp.status must_== Status.SwitchingProtocols
//      resp.headers.get(`Content-Length`) must_== None
//      resp.headers.get(`Transfer-Encoding`) must_== None
//    }
//
//    "Build a redirect response" in {
//      val location = Uri.fromString("/foo").getOrElse(sys.error("Fail."))
//      val result: Result[Status.MovedPermanently.type , EmptyResult] = MovedPermanently(location).run
//      val resp = result.resp
//
//      resp.body.runLog.run.length must_== 0
//      resp.status must_== Status.MovedPermanently
//      resp.headers.get(`Content-Length`) must_== None
//      resp.headers.get(`Transfer-Encoding`) must_== None
//      resp.headers.get(Location) must_== Some(Location(location))
//    }
//
//    "Explicitly added headers have priority" in {
//      val w = Writable[String](toEntity = Writable.stringWritable.toEntity,
//        headers = Writable.stringWritable.headers.put(`Content-Type`(MediaType.`text/html`)))
//         Ok("some content", Headers(`Content-Type`(MediaType.`application/json`)))(w)
//          .run.resp.headers.get(`Content-Type`).get must_== `Content-Type`(MediaType.`application/json`)
//    }
//  }
//}
