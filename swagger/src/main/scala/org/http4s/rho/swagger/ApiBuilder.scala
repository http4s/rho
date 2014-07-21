package org.http4s
package rho
package swagger

import com.typesafe.scalalogging.slf4j.StrictLogging
import com.wordnik.swagger.model._

import org.http4s.rho.bits.HeaderAST.HeaderRule
import bits.Metadata
import bits.PathAST._
import bits.QueryAST.{QueryCapture, QueryRule}

import scala.reflect.runtime.universe.TypeTag

import SwaggerMeta._

import scala.util.Random


class ApiBuilder(apiVersion: String) extends StrictLogging {

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

  def baseOp = Operation("GET", "", "", "void", "temp- will replace", 0)

  def actionToApiListing(action: RhoAction[_, _]): Seq[ApiListing] = {
    val consumes = action.decoders.map(_.value).toList
    val produces = action.responseEncodings.map(_.value).toList

    // Get the result types and models
    val responseClass = action.responseType.map(TypeBuilder.DataType(_).name).getOrElse("void")
    val models = action.responseType.map { tag =>
      TypeBuilder.collectModels(tag, Set.empty)
        .map(model => model.id -> model)
        .toMap
    }

    println(models)

    // Collect the descriptions
    val descriptions = getDescriptions(action.path, action.query)
      .flatMap(runHeaders(action.headers, _))
      .map( desc => desc.copy(operations = desc.operations.map { op =>   // add HTTP Method
           op.copy(method = action.method.toString,
                   nickname = generateNickname(desc.path, action.method),
                   responseClass = responseClass,
                   produces = produces,
                   consumes = consumes)
        }))

    val swaggerVersion = "1.2"
    val basepath = "/"

    descriptions.map { apidesc =>
      val resourcepath = "/" + apidesc.path.split("/").find(!_.isEmpty).getOrElse("")
      ApiListing(apiVersion, swaggerVersion, basepath, resourcepath, models = models, apis = List(apidesc))
    }
  }

  // Generate a nickname for a route. Its simple: 'methodPathStuff' ~ 'getFooBar'
  private def generateNickname(path: String, method: Method): String = {
    method.toString + path.split("/")
                          .filter(s => !s.isEmpty && !(s.startsWith("{") && s.endsWith("}")))
                          .map(_.capitalize)
                          .mkString
  }

  private def getDescriptions(path: PathRule, query: QueryRule): List[ApiDescription] = {
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

      case Nil =>
        val ops = getOperations(Nil, query)
        if (!ops.isEmpty) ops.map { case (path, op) =>
          desc.copy(desc.path + path, operations = List(op))
        }
        else List(desc)
    }

    go(path::Nil, ApiDescription("", None))
  }

  def getOperations(stack: List[PathRule], query: QueryRule, op: Option[Operation] = None): List[(String, Operation)] = {
    def go(stack: List[PathRule], path: String, op: Operation): List[(String, Operation)] = stack match {
      case PathOr(a, b)::xs     => go(a::xs, path, op):::go(b::xs, path, op)
      case PathAnd (a, b) :: xs => go(a::b::xs, path, op)
      case PathMatch(s)::xs     => go(xs, path + "/" + s, op)
      case PathEmpty::xs        => go(xs, path, op)

      case PathCapture (id, parser, _) :: xs =>
        val tpe = parser.typeTag.map(getType).getOrElse("string")
        val p = Parameter (id, None, None, true, false, tpe, AnyAllowableValues, "path", None)
        go(xs, path + s"/{$id}", op.copy(parameters = op.parameters:+p))

      case CaptureTail()::xs =>
        if (!xs.isEmpty) logger.warn(s"Warning: path rule after tail capture: $xs")
        val ps = Parameter ("tail...", None, None, false, true, "string", AnyAllowableValues, "path", None)
        List(path + "/{tail...}" -> op.copy(parameters = op.parameters:::ps::analyzeQuery(query)))

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
      case EmptyQuery::xs => go(xs)
      case QueryOr(a, b)::xs  => go(a::xs):::go(b::xs)

      case (q @ QueryCapture(_, _, _, _))::xs => gatherParam(q)::go(xs)

      case MetaCons(q @ QueryCapture(_, _, _, _), meta)::xs =>
        getMeta(meta).fold(go(q::xs)){ s =>
          gatherParam(q).copy(description = Some(s))::go(xs)
        }

      case MetaCons(a, _)::xs => go(a::xs)

      case Nil => Nil
    }

    go(query::Nil)
  }

  private def gatherParam(rule: QueryCapture[_]): Parameter = {
    Parameter(rule.name, None, rule.default.map(_.toString), rule.default.isEmpty,
      false, getType(rule.m), paramType = "query")
  }

  // Finds any parameters required for the routes and adds them to the descriptions
  private def runHeaders(rule: HeaderRule, desc: ApiDescription): Seq[ApiDescription] = {
    import bits.HeaderAST._

    def addKey(key: HeaderKey.Extractable, desc: ApiDescription): ApiDescription = {
      val p = Parameter(key.name.toString, None, None, true, false, "string", paramType = "header")
      desc.copy(operations = desc.operations.map(op => op.copy(parameters = op.parameters:+p)))
    }

    def go(stack: List[HeaderRule], desc: ApiDescription): List[ApiDescription] = stack match {
      case HeaderAnd(a, b)::xs        => go(a::b::xs, desc)
      case HeaderOr(a, b)::xs         => go(a::xs, desc):::go(b::xs, desc)
      case MetaCons(a, _)::xs         => go(a::xs, desc)
      case EmptyHeaderRule::xs        => go(xs, desc)
      case HeaderCapture(key)::xs     => go(xs, addKey(key, desc))
      case HeaderMapper(key, _)::xs   => go(xs, addKey(key, desc))
      case HeaderRequire(key, _)::xs  => go(xs, addKey(key, desc))
      case Nil                        => desc::Nil
    }

    go(rule::Nil, desc)
  }

//  private def getType(m: TypeTag[_]): String = "string" // TODO: get right type
private def getType(m: TypeTag[_]): String = TypeBuilder.DataType(m).name
}


object SwaggerMeta {

  def getMeta(meta: Metadata): Option[String] = {
    println("'getMeta' undefined")
    None
  }

  sealed trait SwaggerMeta extends Metadata
  case class ApiVersion(version: String) extends SwaggerMeta
}
