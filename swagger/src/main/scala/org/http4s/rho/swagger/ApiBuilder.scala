package org.http4s
package rho
package swagger

import com.wordnik.swagger.models.{Operation, Path}
import com.wordnik.swagger.models.parameters.{PathParameter, QueryParameter, HeaderParameter, Parameter}
import com.wordnik.swagger.{ models => m }

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits.ResponseGenerator.EmptyRe
import org.http4s.rho.bits._
import bits.PathAST._
import bits.QueryAST.{QueryCapture, QueryRule}

import org.log4s.getLogger

import scala.reflect.runtime.universe.{ Type, weakTypeOf }


class ApiBuilder(apiVersion: String, swagger: m.Swagger) {

  private[this] val logger = getLogger

  val swaggerVersion = "2.0"
  val basepath = "/"

  def actionToApiListing(action: RhoAction[_, _]): Unit = {
    // Add the 'consumes' to the Swagger // TODO: these are wrong, and should be on the Operation
    action.validMedia.foreach(media => swagger.addConsumes(media.renderString))
    // Add the 'produces' to the Swagger
    action.responseEncodings.foreach(enc => swagger.produces(enc.renderString))

//    // Get the result types and models
//    val models: m.Model = {
//      val models = action.resultInfo.collect {
//        case TypeOnly(tpe) => tpe
//        case StatusAndType(_, tpe) => tpe
//      }.foldLeft(Set.empty[m.Model]){(s, tpe) =>
//        TypeBuilder.collectModels(tpe, s, formats)
//      }
//      if (models.isEmpty) None
//      else Some(models.map(m => m.id -> m).toMap)
//    }

    // Operation.responses = Map("status" -> Response)

//    val responseMessages = action.resultInfo.toList.collect {
//      case TypeOnly(tpe) => ResponseMessage(200, "OK", Some(TypeBuilder.DataType(tpe).name))
//      case StatusAndType(s, tpe) => ResponseMessage(s.code, s.reason, Some(TypeBuilder.DataType(tpe).name))
//      case StatusOnly(status) => ResponseMessage(status.code, status.reason)
//    }
//
//    val responseClass = responseMessages match {
//      case ResponseMessage(_, _, Some(tpe))::Nil => tpe
//      case _                                     => "mixed result types"
//    }

    // Collect the descriptions
//    val descriptions = getDescriptions(action.path, action.query)
//      .map { desc =>
//        desc.copy(operations = desc.operations.map( op =>
//            op.copy(method           = action.method.toString,
//                    nickname         = generateNickname(desc.path, action.method),
//                    responseClass    = responseClass,  // What does this do?
//                    produces         = produces,
//                    consumes         = consumes,
//                    parameters       = op.parameters:::analyzeHeaders(action.headers),
//                    responseMessages = responseMessages
//            )))
//      }

    collectOperation(action.path::Nil).foreach{ case (str, path) =>
      val op = new Operation
      analyzeQuery(action.query).foreach(op.addParameter)
      analyzeHeaders(action.headers).foreach(op.addParameter)
      path.set(action.method.name.toLowerCase, op)

      swagger.getPath(str) match {
        case p: Path => swagger.path(str, mergePath(p, path))
        case null    => swagger.path(str, path)
      }
    }
  }

  // Generate a nickname for a route. Its simple: 'methodPathStuff' ~ 'getFooBar'
  private[swagger] def generateNickname(path: String, method: Method): String = {
    method.toString.toLowerCase + path.split("/")
                          .filter(s => !s.isEmpty && !(s.startsWith("{") && s.endsWith("}")))
                          .map(_.capitalize)
                          .mkString
  }

  private def mergePath(p1: Path, p2: Path): Path = p1 // TODO: do an actual merge

//  // TODO: This method needs documentation
//  private def getDescriptions(method: Method, path: PathRule, query: QueryRule): List[(String, m.Path)] = {
//    val qparams = analyzeQuery(query)
//
//    def go(stack: List[PathRule], basepath: String): List[(String, m.Path)] = stack match {
//      case PathAnd(a, b)::xs           => go(a::b::xs, basepath)
//      case PathOr(a, b)::xs            => go(a::xs, basepath):::go(b::xs, basepath)
//
//      case PathMatch("")::Nil          => go(Nil, basepath + "/")
//
//      case PathMatch("")::xs           => go(xs, basepath)
//
//      case PathMatch(s)::xs            => go(xs, basepath + "/" + s)
//
//      case stack @ (CaptureTail | PathCapture(_, _, _))::_ =>
//        val p = new Path
//        collectOperation(stack, p).foreach { case (path, op) =>
//          // TODO: what should the path look like?
//          qparams.foreach(op.addParameter)
//          p.set(method.toString.toLowerCase, op)
//        }
//        List(basepath -> p)
//
////      case stack @ MetaCons(a, RouteDesc(meta))::xs =>
////        go(a::xs, basepath).map(path =>
////          path.
////          desc.copy(operations = desc.operations.map(addOpSummary(meta, _)))
////        )
//
//      case stack @ MetaCons(a, _)::xs => go(a::xs, basepath)    // ignore other metadata
//
//      case Nil =>
//        val p = new Path
//        val ops = collectOperation(Nil, p)
//        if (!ops.isEmpty) ops.map { case (path, op) =>
//          val p = new Path
//
//          desc.copy(desc.path + path, operations = List(op))
//        }
//        else List(basepath -> p)
//    }
//
//    go(path::Nil, "")
//  }

  private def concatPath(p1: String, p2: String): String = p1 match {
    case "" => p2
    case p1 => p1 + "/" + p2
  }

  private[swagger] def collectOperation(stack: List[PathRule]): List[(String, m.Path)] = {
    val linearized: List[List[PathOperation]] = {
      def go(stack: List[PathRule], acc: List[PathOperation]): List[List[PathOperation]] = stack match {
        case PathOr(a, b)::xs           => go(a::xs, acc):::go(b::xs, acc)
        case PathAnd (a, b) :: xs       => go(a::b::xs, acc)
        case (m@ MetaCons(a, meta))::xs => go(a::xs, m::acc)
        case (op: PathOperation)::xs    => go(xs, op::acc)

        case Nil => acc::Nil
      }
      go(stack, Nil).map(_.reverse)
    }

    def go(stack: List[PathOperation], pathstr: String, path: m.Path): (String, m.Path) = stack match {
      case PathMatch("")::Nil   => go(Nil, pathstr, path)
      case PathMatch(s)::xs     => go(xs, concatPath(pathstr, s), path)

      case PathCapture (id, parser, _) :: xs =>
        val tpe = parser.typeTag.map(tag => getType(tag.tpe)).getOrElse("string")
        val p = new PathParameter
        p.setName(id)
        p.setRequired(true)
        p.setType(tpe)
        path.addParameter(p)
//        val p2 = Parameter (id, None, None, true, false, tpe, AnyAllowableValues, "path", None)
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
//        val ps = Parameter ("tail...", None, None, false, true, "string", AnyAllowableValues, "path", None)
//        List(path + "/{tail...}" -> op.copy(parameters = op.parameters:::ps::analyzeQuery(query)))

      case MetaCons(_, meta)::xs => // the linearization took care of the consed path
//        meta match {
//          case RouteDesc(meta) => addOpSummary(meta, path)
//          case _               => // NOOP
//        }
        go(xs, pathstr, path)

      case Nil => pathstr -> new Path
//        List(path -> op.copy(parameters = op.parameters:::qs))
    }

    linearized.map{ pathstack =>
      val (pathstr, path) = go(pathstack, "", new Path)
      (pathstr, path)
    }
  }

  // Adds a note that the params are optional if the other params are satisfied
  private def addOrDescriptions(as: List[Parameter], bs: List[Parameter], tpe: String): List[Parameter] = {
    if (bs.isEmpty) as      // These two cases shouldn't happen, but just in case something changes down the road
    else if (as.isEmpty) bs
    else {
      val reqStr = s"Optional if the following $tpe are satisfied: " + bs.map(_.getName()).mkString("[",", ", "]")
      as.foreach { p =>
        if (p.getDescription().length > 0) p.setDescription(p.getDescription() + "; " + reqStr)
        else p.setDescription(reqStr)
      }
      as
    }
  }

  private[swagger] def analyzeQuery(query: QueryRule): List[m.parameters.Parameter] = {
    import bits.QueryAST._
    def go(stack: List[QueryRule]): List[m.parameters.Parameter] = stack match {
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

  private def gatherParam(rule: QueryCapture[_]): m.parameters.Parameter = {
    val p = new QueryParameter
    p.setName(rule.name)
    rule.default.foreach(i => p.setDefaultValue(i.toString))
    p.setRequired(rule.default.isEmpty)
    p.setType(getType(rule.m.tpe))
    p
//    Parameter(rule.name, None, rule.default.map(_.toString), rule.default.isEmpty,
//      false, getType(rule.m.tpe), paramType = "query")
  }

  // Finds any parameters required for the routes and adds them to the descriptions
  private[swagger] def analyzeHeaders(rule: HeaderRule): List[m.parameters.Parameter] = {
    import bits.HeaderAST._

    def mkParam(key: HeaderKey.Extractable): m.parameters.Parameter = {
      val p = new HeaderParameter
      p.setName(key.name.toString())
      p.setRequired(true)
      p.setType("string") //Parameter(key.name.toString, None, None, true, false, "string", paramType = "header")

      p
    }

    def go(stack: List[HeaderRule]): List[m.parameters.Parameter] = stack match {
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
  private def addOpSummary(summary: String, op: m.Operation): Unit = {
    if (op.getSummary != "") logger.warn(s"Overriding route description '${op.getSummary()}' with '$summary'")
    op.setSummary(summary)
  }

  private def getType(m: Type): String = "string" //TypeBuilder.DataType(m).name
}
