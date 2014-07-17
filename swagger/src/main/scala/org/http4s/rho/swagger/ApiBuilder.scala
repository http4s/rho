package org.http4s
package rho
package swagger

import com.wordnik.swagger.model._

import bits.HeaderAST.HeaderRule
import bits.Metadata
import bits.PathAST._
import org.http4s.rho.bits.QueryAST.{QueryCapture, QueryRule}

import scala.reflect.runtime.universe.TypeTag

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

  protected def actionToApiListing(action: RhoAction[_, _]): Seq[ApiListing] = {

    ???
  }

  private def getDescriptions(path: PathRule, query: QueryRule, headers: HeaderRule): List[ApiDescription] = {
    def go(stack: List[PathRule], desc: ApiDescription): List[ApiDescription] = stack match {
      case PathAnd(a, b)::xs           => go(a::b::xs, desc)
      case PathOr(a, b)::xs            => go(a::xs, desc):::go(b::xs, desc)
      case PathMatch(s)::xs            => go(xs, desc.copy(path = desc.path + "/" + s))

      case stack @ (CaptureTail() | PathCapture(_, _, _))::_ =>
        getOperations(stack, query).map{ case (path, op) =>
          desc.copy(desc.path + path, operations = List(op))
        }

      case stack @ MetaCons(CaptureTail() | PathCapture(_, _, _), meta)::_ =>
        val op = getMeta(meta).fold(baseOp){ meta => baseOp.copy(summary = meta) }
        getOperations(stack, query, Some(op)).map { case (path, op) =>
          desc.copy(desc.path + path, operations = List(op))
        }

      case stack @ MetaCons(a, meta)::xs =>
        getMeta(meta) match {
          case m @ Some(meta) => desc.description match {
            case Some(d) => go(a::xs, desc.copy(description = Some(desc + "\n" + d)))
            case None       => go(a::xs, desc.copy(description = m))
          }         // need to see if we are working on Parameters
          case None       => go(a::xs, desc)
        }

      case PathEmpty::Nil => List(desc.copy(operations = baseOp::Nil))

      case PathEmpty::xs  => go(xs, desc)

      case Nil => Nil
    }

    go(path::Nil, ApiDescription("", None))
      .map(runHeaders(headers, _))            // Add any info in the headers
  }

  def getOperations(stack: List[PathRule], query: QueryRule, op: Option[Operation] = None): List[(String, Operation)] = {
    def go(stack: List[PathRule], path: String, op: Operation): List[(String, Operation)] = stack match {
      case PathOr(a, b)::xs     => go(a::xs, path, op):::go(b::xs, path, op)
      case PathAnd (a, b) :: xs => go(a::b::xs, path, op)
      case PathMatch(s)::xs     => go(xs, path + "/" + s, op)
      case PathEmpty::xs        => go(xs, path, op)

      case PathCapture (id, parser, _) :: xs =>
        val p = Parameter (id, None, None, true, false,
        parser.typeTag.map (_.tpe.toString).getOrElse ("none"),
        AnyAllowableValues, "path", None)
        go(xs, path + s"/{$id}", op.copy(parameters = op.parameters:+p))

      case CaptureTail()::xs =>
        val ps = Parameter ("variadic", None, None, false, true, "array",
                           AnyAllowableValues, "path", None)::analyzeQuery(query)
        val _path = path + "/..."
        List(path -> op.copy(parameters = op.parameters:::ps))

      case MetaCons(rule, meta)::xs =>
        getMeta(meta) match {
          case Some(meta) => go(rule::xs, path, op.copy(notes = op.notes + "\n" + meta))
          case None       => go(rule::xs, path, op)
        }

      case Nil =>
        val qs = analyzeQuery(query)
        List(path -> op.copy(parameters = op.parameters:::qs))
    }
    go(stack, "", op.getOrElse(baseOp))
  }

  def analyzeQuery(query: QueryRule): List[Parameter] = {
    import bits.QueryAST._
    def go(stack: List[QueryRule]): List[Parameter] = stack match {
      case QueryAnd(a, b)::xs => go(a::b::xs)

      case QueryOr(a, b)::xs  =>
        go(a::xs):::go(b::xs).map(_.copy(required = false))

      case (q @ QueryCapture(_, _, _, _))::xs => gatherParam(q)::go(xs)

      case MetaCons(q @ QueryCapture(_, _, _, _), meta)::xs =>
        getMeta(meta).fold(go(q::xs)){ s =>
          gatherParam(q).copy(description = Some(s))::go(xs)
        }

      case EmptyQuery::xs => go(xs)

      case Nil => Nil
    }

    go(query::Nil)
  }

  private def gatherParam(rule: QueryCapture[_]): Parameter = {
    Parameter(rule.name, None, rule.default.map(_.toString), rule.default.isEmpty,
      false, getType(rule.m), paramType = "query")
  }

  private def runHeaders(rule: HeaderRule, desc: ApiDescription): ApiDescription = ???

  private def produces(rule: HeaderRule): List[String] = ???

  private def consumes(rule: HeaderRule): List[String] = ???

  private def getType(m: TypeTag[_]): String = ???
}


object SwaggerMeta {

  def getMeta(meta: Metadata): Option[String] = ???

  sealed trait SwaggerMeta extends Metadata
  case class ApiVersion(version: String) extends SwaggerMeta
}