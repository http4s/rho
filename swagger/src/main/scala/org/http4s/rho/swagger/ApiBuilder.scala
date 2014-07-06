package org.http4s
package rho
package swagger

import com.wordnik.swagger.model._

import bits.HeaderAST.HeaderRule
import bits.Metadata
import bits.PathAST._
import org.http4s.rho.bits.QueryAST.QueryRule

import shapeless.HList

import scala.collection.mutable.ListBuffer

import SwaggerMeta._


trait ApiBuilder { self: RhoService =>

  /* swagger-core models
  case class ApiListing (
    apiVersion: String,
    swaggerVersion: String,
    basePath: String,
    resourcePath: String,
    produces: List[String] = List.empty,
    consumes: List[String] = List.empty,
    protocols: List[String] = List.empty,
    authorizations: List[Authorization] = List.empty,
    apis: List[ApiDescription] = List(),--------
    models: Option[Map[String, Model]] = None, |
    description: Option[String] = None,        |
    position: Int = 0)                         |
                                               |
  case class ApiDescription ( -----------------|
    path: String,
    description: Option[String],
    operations: List[Operation] = List()) ----
                                             |
  case class Operation ( --------------------|
    method: String,
    summary: String,
    notes: String,
    responseClass: String,
    nickname: String,
    position: Int,
    produces: List[String] = List.empty,
    consumes: List[String] = List.empty,
    protocols: List[String] = List.empty,
    authorizations: List[Authorization] = List.empty,
    parameters: List[Parameter] = List.empty, -------------
    responseMessages: List[ResponseMessage] = List.empty, |
    `deprecated`: Option[String] = None)                  |
                                                          |
  case class Parameter ( ---------------------------------|
    name: String,
    description: Option[String],
    defaultValue: Option[String],
    required: Boolean,
    allowMultiple: Boolean,
    dataType: String,
    allowableValues: AllowableValues = AnyAllowableValues,
    paramType: String,
    paramAccess: Option[String] = None)
   */

  def baseOp = Operation("GET", "", "", "", "", 0)

  protected def actionToApiListing(action: RhoAction[_, _]): (String, Seq[ApiListing]) = {
    val path = new StringBuilder


    ???
  }

  case class Route(path: List[PathRule[_ <: HList]],
                   query: List[QueryRule[_ <: HList]],
                   headers: List[HeaderRule[_ <: HList]]) {
    def withPath(path: List[PathRule[_ <: HList]]): Route = this.copy(path = path)
    def withQuery(query: List[QueryRule[_ <: HList]]): Route = this.copy(query = query)
    def withHeaders(headers: List[HeaderRule[_ <: HList]]): Route = this.copy(headers = headers)
  }

  private def buildApi(base: ApiListing): List[ApiListing] = ???

  private def getDescriptions(route: Route): List[ApiDescription] = {

    // This is to deal with variance problems and destructuring
    def pathMeta(path: PathRule[_ <: HList]): Boolean = path match {
      case CaptureTail() | PathCapture(_,_) => false
      case _                                => true
    }

    def go(stack: List[PathRule[_ <: HList]], desc: ApiDescription): List[ApiDescription] = stack match {
      case PathAnd(a, b)::xs           => go(a::b::xs, desc)
      case PathOr(a, b)::xs            => go(a::xs, desc):::go(b::xs, desc)
      case PathMatch(s)::xs            => go(xs, desc.copy(path = desc.path + "/" + s))

      case stack @ (CaptureTail() | PathCapture(_, _))::_ =>
        getOperations(route.withPath(stack)).map{ case (path, op) =>
          desc.copy(desc.path + path, operations = List(op))
        }

      case stack @ MetaCons(a, meta)::xs =>
        if (pathMeta(a)) getMeta(meta) match {
          case m @ Some(meta) => desc.description match {
            case Some(d) => go(a::xs, desc.copy(description = Some(desc + "\n" + d)))
            case None       => go(a::xs, desc.copy(description = m))
          }         // need to see if we are working on Parameters
          case None       => go(a::xs, desc)
        }
        else getOperations(route.withPath(stack)).map{ case (path, op) =>
          desc.copy(desc.path + path, operations = List(op))
        }

      case PathEmpty::Nil => List(desc.copy(operations = baseOp::Nil))

      case PathEmpty::xs  => go(xs, desc)

      case Nil => Nil
    }

    go(route.path, ApiDescription("", None))
  }

  def getOperations(route: Route): List[(String, Operation)] = {
    def go(stack: List[PathRule[_ <: HList]], path: String, op: Operation): List[(String, Operation)] = stack match {
      case PathOr(a, b)::xs     => go(a::xs, path, op):::go(b::xs, path, op)
      case PathAnd (a, b) :: xs => go(a::b::xs, path, op)

      case PathCapture (id, parser) :: xs =>
        val p = Parameter (id, None, None, true, false,
        parser.manifest.map (_.runtimeClass.getName).getOrElse ("none"),
        AnyAllowableValues, "path", None)
        go(xs, path + s"/{$id}", op.copy(parameters = op.parameters:+p))

      case CaptureTail()::xs =>
        val p = Parameter ("variadic", None, None, false, true,
                           "array", AnyAllowableValues, "path", None)
        val _op = runQuery(route.query, op.copy(parameters = op.parameters:+p))
        val _path = path + "/..."
        List(_path -> _op)

    }
    go(route.path, "", baseOp)
  }

  def runQuery(query: List[QueryRule[_ <: HList]], op: Operation): Operation = ???

//  private def getPathOps(path: List[PathRule[_ <: HList]],
//                         query: QueryRule[_ <: HList],
//                         headers: HeaderRule[_ <: HList],
//                         base: Operation): List[Operation] = {
//    path match {
//      case PathAnd(a, b)::xs  => getPathOps(a::b::xs, query, headers, base)
//
//      case PathOr(a, b)::xs   => getPathOps(a::xs, query, headers, base):::getPathOps(b::xs, query, headers, base)
//
//      case PathCapture(id, parser)::xs =>
//        val p = Parameter(id, None, None, true, false,
//          parser.manifest.map(_.runtimeClass.getName).getOrElse("none"),
//          AnyAllowableValues, "path", None)
//
//        getPathOps(xs, query, headers, base.copy(parameters = base.parameters:+ p))
//
//      case MetaCons(a, meta)::xs =>
//        getMeta(meta) match {
//          case Some(meta) => getPathOps(xs, query, headers, base.copy(notes = base.notes + "\n" + meta))
//          case None       => getPathOps(xs, query, headers, base)
//        }
//
//      case CaptureTail()::xs =>
//        if (!xs.isEmpty) logger.warn(s"Invalid path structure: CaptureTail followed by $xs")
//        val p = Parameter("variadic", None, None, false, true,
//          "array", AnyAllowableValues, "path", None)
//        List(base.copy(parameters = base.parameters :+ p))
//
//      case PathEmpty::xs => getPathOps(xs, query, headers, base)  // This is not expected
//
//      case PathMatch(s)::xs => ??? // this is a strange structure
//    }
//  }

  private def produces(rule: HeaderRule[_]): List[String] = ???

  private def consumes(rule: HeaderRule[_]): List[String] = ???
}


object SwaggerMeta {

  def getMeta(meta: Metadata): Option[String] = ???

  sealed trait SwaggerMeta extends Metadata
  case class ApiVersion(version: String) extends SwaggerMeta
}