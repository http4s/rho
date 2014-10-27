package org.http4s
package rho
package swagger

import com.typesafe.scalalogging.slf4j.StrictLogging
import com.wordnik.swagger.model._

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits._
import bits.PathAST._
import bits.QueryAST.{QueryCapture, QueryRule}

import scala.reflect.runtime.universe.Type


class ApiBuilder(apiVersion: String, formats: SwaggerFormats) extends StrictLogging {

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

  val swaggerVersion = "1.2"
  val basepath = "/"

  def baseOp = Operation("GET", "", "", "void", "temp- will replace", 0)

  def actionToApiListing(action: RhoAction[_, _]): Seq[ApiListing] = {
    val consumes = action.validMedia.map(_.renderString).toList
    val produces = action.responseEncodings.map(_.renderString).toList

    // Get the result types and models
    val models = {
      val models = action.resultInfo.collect {
        case ModelOnly(tpe) => tpe
        case StatusAndModel(_, tpe) => tpe
      }.foldLeft(Set.empty[Model]){(s, tpe) =>
        TypeBuilder.collectModels(tpe, Set.empty, formats)
      }
      if (models.isEmpty) None
      else Some(models.map(m => m.id -> m).toMap)
    }

    val responseMessages = action.resultInfo.toList.collect {
      case ModelOnly(tpe) => ResponseMessage(200, "OK", Some(TypeBuilder.DataType(tpe).name))
      case StatusAndModel(s, tpe) => ResponseMessage(s.code, s.reason, Some(TypeBuilder.DataType(tpe).name))
      case StatusOnly(status) => ResponseMessage(status.code, status.reason)
    }

    val responseClass = responseMessages match {
      case ResponseMessage(_, _, Some(tpe))::Nil => tpe
      case _                                     => "mixed result types"
    }

    // Collect the descriptions
    val descriptions = getDescriptions(action.path, action.query)
      .map { desc =>
        desc.copy(operations = desc.operations.map( op =>
            op.copy(method           = action.method.toString,
                    nickname         = generateNickname(desc.path, action.method),
                    responseClass    = responseClass,  // What does this do?
                    produces         = produces,
                    consumes         = consumes,
                    parameters       = op.parameters:::analyzeHeaders(action.headers),
                    responseMessages = responseMessages
            )))
      }

    descriptions.map { apidesc =>
      val resourcepath = "/" + apidesc.path.split("/").find(!_.isEmpty).getOrElse("")
      ApiListing(apiVersion, swaggerVersion, basepath, resourcepath, models = models, apis = List(apidesc))
    }
  }

  // Generate a nickname for a route. Its simple: 'methodPathStuff' ~ 'getFooBar'
  private[swagger] def generateNickname(path: String, method: Method): String = {
    method.toString.toLowerCase + path.split("/")
                          .filter(s => !s.isEmpty && !(s.startsWith("{") && s.endsWith("}")))
                          .map(_.capitalize)
                          .mkString
  }

  // TODO: This method needs documentation
  private def getDescriptions(path: PathRule, query: QueryRule): List[ApiDescription] = {
    def go(stack: List[PathRule], desc: ApiDescription): List[ApiDescription] = stack match {
      case PathAnd(a, b)::xs           => go(a::b::xs, desc)
      case PathOr(a, b)::xs            => go(a::xs, desc):::go(b::xs, desc)
      case PathMatch(s)::xs            => go(xs, desc.copy(path = desc.path + "/" + s))

      case stack @ (CaptureTail | PathCapture(_, _, _))::_ =>
        collectPaths(stack, query, baseOp).map{ case (path, op) =>
          desc.copy(desc.path + path, operations = List(op))
        }

      case stack @ MetaCons(a, RouteDesc(meta))::xs =>
        go(a::xs, desc).map(desc =>
          desc.copy(operations = desc.operations.map(addOpSummary(meta, _)))
        )

      case stack @ MetaCons(a, _)::xs => go(a::xs, desc)    // ignore other metadata

      case PathEmpty::Nil => List(desc.copy(operations = baseOp::Nil))

      case PathEmpty::xs  => go(xs, desc)

      case Nil =>
        val ops = collectPaths(Nil, query, baseOp)
        if (!ops.isEmpty) ops.map { case (path, op) =>
          desc.copy(desc.path + path, operations = List(op))
        }
        else List(desc)
    }

    go(path::Nil, ApiDescription("", None))
  }

  private[swagger] def collectPaths(stack: List[PathRule], query: QueryRule, op: Operation): List[(String, Operation)] = {
    def go(stack: List[PathRule], path: String, op: Operation): List[(String, Operation)] = stack match {
      case PathOr(a, b)::xs     => go(a::xs, path, op):::go(b::xs, path, op)
      case PathAnd (a, b) :: xs => go(a::b::xs, path, op)
      case PathMatch(s)::xs     => go(xs, path + "/" + s, op)
      case PathEmpty::xs        => go(xs, path, op)

      case PathCapture (id, parser, _) :: xs =>
        val tpe = parser.typeTag.map(tag => getType(tag.tpe)).getOrElse("string")
        val p = Parameter (id, None, None, true, false, tpe, AnyAllowableValues, "path", None)
        go(xs, s"$path/{$id}", op.copy(parameters = op.parameters:+p))

      case CaptureTail::xs =>
        if (!xs.isEmpty) logger.warn(s"Warning: path rule after tail capture: $xs")
        val ps = Parameter ("tail...", None, None, false, true, "string", AnyAllowableValues, "path", None)
        List(path + "/{tail...}" -> op.copy(parameters = op.parameters:::ps::analyzeQuery(query)))

      case MetaCons(rule, meta)::xs =>
        val op2 = meta match {
          case RouteDesc(meta) => addOpSummary(meta, op)
          case _               => op
        }
        go(rule::xs, path, op2)

      case Nil =>
        val qs = analyzeQuery(query)
        List(path -> op.copy(parameters = op.parameters:::qs))
    }
    go(stack, "", op)
  }

  // Adds a note that the params are optional if the other params are satisfied
  private def addOrDescriptions(as: List[Parameter], bs: List[Parameter], tpe: String): List[Parameter] = {
    if (bs.isEmpty) as      // These two cases shouldn't happen, but just in case something changes down the road
    else if (as.isEmpty) bs
    else {
      val reqStr = s"Optional if the following $tpe are satisfied: " + bs.map(_.name).mkString("[",", ", "]")
      as.map(p => p.copy(description = Some({ p.description.map(_ + "; ").getOrElse("") + reqStr})))
    }
  }

  private[swagger] def analyzeQuery(query: QueryRule): List[Parameter] = {
    import bits.QueryAST._
    def go(stack: List[QueryRule]): List[Parameter] = stack match {
      case QueryAnd(a, b)::xs => go(a::b::xs)
      case EmptyQuery::xs => go(xs)
      case QueryOr(a, b)::xs  =>
        val as = go(a::xs)
        val bs = go(b::xs)
        addOrDescriptions(as, bs, "params"):::addOrDescriptions(bs, as, "params")


      case (q @ QueryCapture(_, _, _, _))::xs => gatherParam(q)::go(xs)

      case MetaCons(q @ QueryCapture(_, _, _, _), meta)::xs => meta match {
        case m: TextMetaData => gatherParam(q).copy(description = Some(m.msg))::go(xs)
        case _               => go(q::xs)
      }

      case MetaCons(a, _)::xs => go(a::xs)

      case Nil => Nil
    }

    go(query::Nil)
  }

  private def gatherParam(rule: QueryCapture[_]): Parameter = {
    Parameter(rule.name, None, rule.default.map(_.toString), rule.default.isEmpty,
      false, getType(rule.m.tpe), paramType = "query")
  }

  // Finds any parameters required for the routes and adds them to the descriptions
  private[swagger] def analyzeHeaders(rule: HeaderRule): List[Parameter] = {
    import bits.HeaderAST._

    def mkParam(key: HeaderKey.Extractable): Parameter = {
      Parameter(key.name.toString, None, None, true, false, "string", paramType = "header")
    }

    def go(stack: List[HeaderRule]): List[Parameter] = stack match {
      case HeaderAnd(a, b)::xs        => go(a::b::xs)
      case MetaCons(a, _)::xs         => go(a::xs)
      case EmptyHeaderRule::xs        => go(xs)
      case HeaderCapture(key)::xs     => mkParam(key)::go(xs)
      case HeaderMapper(key, _)::xs   => mkParam(key)::go(xs)
      case HeaderRequire(key, _)::xs  => mkParam(key)::go(xs)
      case HeaderOr(a, b)::xs         =>
        val as = go(a::xs)
        val bs = go(b::xs)
        addOrDescriptions(as, bs, "headers"):::addOrDescriptions(bs, as, "headers")

      case Nil                        => Nil
    }

    go(rule::Nil)
  }

  // Utility methods
  private def addOpSummary(summary: String, op: Operation): Operation = {
    if (op.summary != "") logger.warn(s"Overriding route description '${op.summary}' with '$summary'")
    op.copy(summary = summary)
  }

  private def getType(m: Type): String = TypeBuilder.DataType(m).name
}
