package org.http4s
package rho
package swagger

import com.wordnik.swagger.models.{Swagger, Model, ModelImpl, RefModel, Operation, Path, Response}
import com.wordnik.swagger.models.parameters.{BodyParameter, PathParameter, QueryParameter, HeaderParameter, Parameter}
import com.wordnik.swagger.models.properties.{RefProperty}

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits.ResponseGenerator.EmptyRe
import org.http4s.rho.bits._
import bits.PathAST._
import bits.QueryAST.{QueryCapture, QueryRule}

import org.log4s.getLogger

import scala.reflect.runtime.universe.{ Type, weakTypeOf }

class ApiBuilder(apiVersion: String, swagger: Swagger, formats: SwaggerFormats) {

  private[this] val logger = getLogger

  val swaggerVersion = "2.0"

  def actionToApiListing(action: RhoAction[_, _]): Unit = {

    val models = {
      val types = action.resultInfo.collect {
        case TypeOnly(tpe) => tpe
        case StatusAndType(_, tpe) => tpe
      } ++ (action.router match {
        case r: CodecRouter[_, _] => Set(r.entityType)
        case _ => Set.empty
      })

      val models = types.foldLeft(Set.empty[Model]) { (s, tpe) =>
        TypeBuilder.collectModels(tpe, s, formats)
      }

      models.map(m => m.getDescription -> m).toMap
    }

    models.foreach { case (name, model) => swagger.model(name, model) }

    collectPaths(action.path::Nil).foreach { case (pathstr, path) =>
      val method = action.method.name.toLowerCase
      swagger.getPath(pathstr) match {
        case p: Path =>
          p.set(method, mkOperation(pathstr, action))
        case null =>
          path.set(method, mkOperation(pathstr, action))
          swagger.path(pathstr, path)
      }
    }
  }

  private[swagger] def mkOperation(pathstr: String, action: RhoAction[_, _]): Operation = {
    val op = new Operation

    analyzeQuery(action.query).foreach(op.addParameter)
    analyzeHeaders(action.headers).foreach(op.addParameter)
    getOpSummary(action.path::Nil).foreach(op.summary)

    op.operationId(mkOperationId(pathstr, action.method))
    op.tag(pathstr.split("/").filterNot(_ == "").head)

    action.validMedia.foreach(media => op.addConsumes(media.renderString))
    action.responseEncodings.foreach(enc => op.produces(enc.renderString))

    action.router match {
      case r: CodecRouter[_, _] =>
        val name = r.entityType.simpleName
        op.addParameter((new BodyParameter).name("body").description(name).schema(new RefModel(name)))
      case _ =>
    }

    action.resultInfo.foreach {
      case TypeOnly(tpe) =>
        op.response(200, (new Response).description("OK").schema(new RefProperty(tpe.simpleName)))
      case StatusAndType(s, tpe) =>
        op.response(s.code, (new Response).description(s.reason).schema(new RefProperty(tpe.simpleName)))
      case StatusOnly(s) =>
        op.response(s.code, (new Response).description(s.reason))
      case Empty =>
    }

    op
  }

  private[swagger] def mkOperationId(path: String, method: Method): String = {
    method.toString.toLowerCase +
    path.split("/")
      .filter(s => !s.isEmpty && !(s.startsWith("{") && s.endsWith("}")))
      .map(_.capitalize)
      .mkString
  }

  private def concatPath(p1: String, p2: String): String = p1 match {
    case "" => p2
    case p1 => p1 + "/" + p2
  }

  private def linearizeStack(stack: List[PathRule]): List[List[PathOperation]] = {
    def go(stack: List[PathRule], acc: List[PathOperation]): List[List[PathOperation]] = stack match {
      case PathOr(a, b)::xs           => go(a::xs, acc):::go(b::xs, acc)
      case PathAnd(a, b) :: xs        => go(a::b::xs, acc)
      case (m@ MetaCons(a, meta))::xs => go(a::xs, m::acc)
      case (op: PathOperation)::xs    => go(xs, op::acc)
      case Nil                        => acc::Nil
    }
    go(stack, Nil).map(_.reverse)
  }

  private[swagger] def collectPaths(stack: List[PathRule]): List[(String, Path)] = {

    def go(stack: List[PathOperation], pathstr: String, path: Path): (String, Path) = stack match {
      case PathMatch("")::Nil   => go(Nil, pathstr, path)
      case PathMatch(s)::xs     => go(xs, concatPath(pathstr, s), path)

      case PathCapture(id, parser, _) :: xs =>
        val tpe = parser.typeTag.map(tag => getType(tag.tpe)).getOrElse("string")
        val p = new PathParameter
        p.setName(id)
        p.setRequired(true)
        p.setType(tpe)
        path.addParameter(p)
        go(xs, s"$pathstr/{$id}", path)

      case CaptureTail::xs =>
        if (!xs.isEmpty) logger.warn(s"Warning: path rule after tail capture: $xs")
        val p = new PathParameter
        p.setName("tail...")
        p.setRequired(false)
        p.setType("string")
        val path = new Path
        path.addParameter(p)
        pathstr + "/{tail...}" -> path

      case MetaCons(_, meta)::xs => go(xs, pathstr, path)

      case Nil => pathstr -> path
    }

    linearizeStack(stack).map(go(_, "", new Path))
  }

  private[swagger] def getOpSummary(stack: List[PathRule]): Option[String] = {

    def go(stack: List[PathOperation], summary: Option[String]): Option[String] = stack match {
      case PathMatch("")::Nil               => go(Nil, summary)
      case PathMatch(s)::xs                 => go(xs, summary)
      case PathCapture(id, parser, _) :: xs => go(xs, summary)
      case CaptureTail::xs                  => summary

      case MetaCons(_, meta)::xs =>
        meta match {
          case RouteDesc(meta) => Some(meta)
          case _               => go(xs, summary)
        }

      case Nil => summary
    }

    linearizeStack(stack).flatMap(go(_, None)).headOption
  }

  // Adds a note that the params are optional if the other params are satisfied
  private def addOrDescriptions(as: List[Parameter], bs: List[Parameter], tpe: String): List[Parameter] = {
    if (bs.isEmpty) as      // These two cases shouldn't happen, but just in case something changes down the road
    else if (as.isEmpty) bs
    else {
      val reqStr = s"Optional if the following $tpe are satisfied: " + bs.map(_.getName()).mkString("[",", ", "]")
      as.foreach { p =>
        if (Option(p.getDescription).isDefined) p.setDescription(p.getDescription() + "; " + reqStr)
        else p.setDescription(reqStr)
      }
      as
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
        case m: TextMetaData =>
          val p = gatherParam(q)
          p.setDescription(m.msg)
          p::go(xs)

        case _               => go(q::xs)
      }

      case MetaCons(a, _)::xs => go(a::xs)

      case Nil => Nil
    }

    go(query::Nil)
  }

  private def gatherParam(rule: QueryCapture[_]): Parameter = {
    val p = new QueryParameter
    p.setName(rule.name)
    rule.default.foreach(i => p.setDefaultValue(i.toString))
    p.setRequired(rule.default.isEmpty)
    p.setType(getType(rule.m.tpe))
    p
  }

  // Finds any parameters required for the routes and adds them to the descriptions
  private[swagger] def analyzeHeaders(rule: HeaderRule): List[Parameter] = {
    import bits.HeaderAST._

    def mkParam(key: HeaderKey.Extractable): Parameter = {
      val p = new HeaderParameter
      p.setName(key.name.toString)
      p.setRequired(true)
      p.setType("string")
      p
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

  private def getType(m: Type): String =
    TypeBuilder.DataType(m).name
}
