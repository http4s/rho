package org.http4s
package rho
package swagger

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.QueryAST.{QueryCapture, QueryRule}
import org.http4s.rho.bits.ResponseGenerator.EmptyRe
import org.http4s.rho.bits._

import org.log4s.getLogger

import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

import scalaz._, Scalaz._

private[swagger] class SwaggerModelsBuilder(formats: SwaggerFormats) {
  import models._

  private[this] val logger = getLogger

  def mkSwagger(info: Info, ra: RhoAction[_, _])(s: Swagger): Swagger =
    Swagger(
      info        = info.some,
      paths       = collectPaths(ra)(s),
      definitions = collectDefinitions(ra)(s))

  def collectPaths(ra: RhoAction[_, _])(s: Swagger): Map[String, Path] = {
    val pairs = mkPathStrs(ra).map { ps =>
      val o = mkOperation(ps, ra)
      val p0 = s.paths.get(ps).getOrElse(Path())
      val p1 = ra.method.name.toLowerCase match {
        case "get"     => p0.copy(get = o.some)
        case "put"     => p0.copy(put = o.some)
        case "post"    => p0.copy(post = o.some)
        case "delete"  => p0.copy(delete = o.some)
        case "patch"   => p0.copy(patch = o.some)
        case "options" => p0.copy(options = o.some)
      }
      ps -> p1
    }
    pairs.foldLeft(s.paths) { case (paths, (s, p)) => paths.alter(s)(_ => p.some) }
  }

  def collectDefinitions(ra: RhoAction[_, _])(s: Swagger): Map[String, Model] = {
    val initial: Set[Model] = s.definitions.values.toSet
    (collectResultTypes(ra) ++ collectCodecTypes(ra))
      .foldLeft(initial)((s, tpe) => s ++ TypeBuilder.collectModels(tpe, s, formats))
      .map(m => m.id2 -> m)
      .toMap
  }

  def collectResultTypes(ra: RhoAction[_, _]): Set[Type] =
    ra.resultInfo.collect {
      case TypeOnly(tpe)         => tpe
      case StatusAndType(_, tpe) => tpe
    }

  def collectCodecTypes(ra: RhoAction[_, _]): Set[Type] =
    ra.router match {
      case r: CodecRouter[_, _] => Set(r.entityType)
      case _                    => Set.empty
    }

  def mkPathStrs(ra: RhoAction[_, _]): List[String] = {

    def go(stack: List[PathOperation], pathStr: String): String =
      stack match {
        case Nil                       => pathStr.isEmpty.fold("/", pathStr)
        case PathMatch("")::xs         => go(xs, pathStr)
        case PathMatch(s)::xs          => go(xs, pathStr + "/" + s)
        case MetaCons(_, _)::xs        => go(xs, pathStr)
        case PathCapture(id, p, _)::xs => go(xs, s"$pathStr/{$id}")
        case CaptureTail::xs           => pathStr + "/{tail...}"
      }

    linearizeStack(ra.path::Nil).map(go(_, ""))
  }

  def collectPathParams(ra: RhoAction[_, _]): List[PathParameter] = {

    def go(stack: List[PathOperation], pps: List[PathParameter]): List[PathParameter] =
      stack match {
        case Nil                       => pps
        case PathMatch("")::xs         => go(xs, pps)
        case PathMatch(s)::xs          => go(xs, pps)
        case MetaCons(_, _)::xs        => go(xs, pps)
        case PathCapture(id, p, _)::xs => go(xs, mkPathParam(id, p)::pps)
        case CaptureTail::xs           => PathParameter(`type` = "string", name = "tail...".some):: Nil
      }

    linearizeStack(ra.path::Nil).map(go(_, Nil)).flatten
  }

  def collectBodyParams(ra: RhoAction[_, _]): Option[BodyParameter] =
    ra.router match {
      case r: CodecRouter[_, _] => mkBodyParam(r).some
      case _                    => none
    }  

  def collectResponses(ra: RhoAction[_, _]): Map[String, Response] =
    ra.resultInfo.collect {
      case TypeOnly(tpe)         => mkResponse("200", "OK", tpe.some).some
      case StatusAndType(s, tpe) => mkResponse(s.code.toString, s.reason, tpe.some).some
      case StatusOnly(s)         => mkResponse(s.code.toString, s.reason, none).some
    }.flatten.toMap

  def collectSummary(ra: RhoAction[_, _]): Option[String] = {

    def go(stack: List[PathOperation], summary: Option[String]): Option[String] =
      stack match {
        case PathMatch("")::Nil             => go(Nil, summary)
        case PathMatch(s)::xs               => go(xs, summary)
        case PathCapture(id, parser, _)::xs => go(xs, summary)
        case CaptureTail::xs                => summary

        case MetaCons(_, meta)::xs =>
          meta match {
            case RouteDesc(meta) => meta.some
            case _               => go(xs, summary)
          }

        case Nil => summary
      }

    linearizeStack(ra.path::Nil).flatMap(go(_, None)).headOption
  }

  def collectOperationParams(ra: RhoAction[_, _]): List[Parameter] =
    collectPathParams(ra) ::: collectQueryParams(ra) ::: collectHeaderParams(ra) ::: collectBodyParams(ra).toList

  def collectQueryParams(ra: RhoAction[_, _]): List[QueryParameter] = {
    import bits.QueryAST._

    def go(stack: List[QueryRule]): List[QueryParameter] =
      stack match {
        case QueryAnd(a, b)::xs => go(a::b::xs)
        case EmptyQuery::xs     => go(xs)

        case QueryOr(a, b)::xs =>
          val as = go(a::xs)
          val bs = go(b::xs)
          val set: (QueryParameter, String) => QueryParameter =
            (p, s) => p.copy(description = p.description.map(_ + s).orElse(s.some))

          addOrDescriptions(set)(as, bs, "params") :::
          addOrDescriptions(set)(bs, as, "params")

        case (q @ QueryCapture(_, _, _, _))::xs => mkQueryParam(q)::go(xs)

        case MetaCons(q @ QueryCapture(_, _, _, _), meta)::xs =>
          meta match {
            case m: TextMetaData => mkQueryParam(q).copy(description = m.msg.some) :: go(xs)
            case _               => go(q::xs)
          }

        case MetaCons(a, _)::xs => go(a::xs)

        case Nil => Nil
      }

    go(ra.query::Nil)
  }

  def collectHeaderParams(ra: RhoAction[_, _]): List[HeaderParameter] = {
    import bits.HeaderAST._

    def go(stack: List[HeaderRule]): List[HeaderParameter] =
      stack match {
        case HeaderAnd(a,b)::xs         => go(a::b::xs)
        case MetaCons(a,_)::xs          => go(a::xs)
        case EmptyHeaderRule::xs        => go(xs)
        case HeaderCapture(key,_,_)::xs  => mkHeaderParam(key)::go(xs)
        case HeaderExists(key,_)::xs   => mkHeaderParam(key)::go(xs)

        case HeaderOr(a, b)::xs         =>
          val as = go(a::xs)
          val bs = go(b::xs)
          val set: (HeaderParameter, String) => HeaderParameter =
            (p, s) => p.copy(description = p.description.map(_ + s).orElse(s.some))
          addOrDescriptions(set)(as, bs, "headers") :::
          addOrDescriptions(set)(bs, as, "headers")

        case Nil                        => Nil
      }

    go(ra.headers::Nil)
  }

  def mkOperation(pathStr: String, ra: RhoAction[_, _]): Operation =
    Operation(
      tags        = pathStr.split("/").filterNot(_ == "").headOption.getOrElse("/") :: Nil,
      summary     = collectSummary(ra),
      consumes    = ra.validMedia.toList.map(_.renderString),
      produces    = ra.responseEncodings.toList.map(_.renderString),
      operationId = mkOperationId(pathStr, ra.method).some,
      parameters  = collectOperationParams(ra),
      responses   = collectResponses(ra))

  def mkOperationId(path: String, method: Method): String = {
    method.toString.toLowerCase +
    path.split("/")
      .filter(s => !s.isEmpty && !(s.startsWith("{") && s.endsWith("}")))
      .map(_.capitalize)
      .mkString
  }

  def mkBodyParam(r: CodecRouter[_, _]): BodyParameter = {
    val tpe = r.entityType
    val model = if (tpe.isPrimitive) {
      val name = TypeBuilder.DataType(tpe).name
      ModelImpl(id = tpe.fullName, id2 = name, `type` = name.some, isSimple = true)
    } else RefModel(tpe.fullName, tpe.fullName, tpe.simpleName)
    BodyParameter(
      schema      = model.some,
      name        = "body".some,
      description = tpe.simpleName.some)
  }

  def mkPathParam(name: String, parser: StringParser[_]): PathParameter = {
    val tpe = parser.typeTag.map(tag => getType(tag.tpe)).getOrElse("string")
    PathParameter(`type` = tpe, name = name.some, required = true)
  }

  def mkResponse(code: String, descr: String, otpe: Option[Type]): (String, Response) = {

    def typeToProp(tpe: Type): Option[Property] =
      if (Reflector.isExcluded(tpe))
        None
      else if (tpe.isPrimitive)
        mkPrimitiveProperty(tpe).some
      else if (tpe.isCollection)
        mkCollectionProperty(tpe)
      else if (tpe.isProcess)
        typeToProp(tpe.dealias.typeArgs(1))
      else if (tpe.isTask)
        typeToProp(tpe.dealias.typeArgs(0))
      else
        RefProperty(ref = tpe.simpleName).some      

    def mkPrimitiveProperty(tpe: Type): Property = {
      import TypeBuilder._
      DataType.fromType(tpe) match {
        case DataType.ValueDataType(name, format, qName) =>
          AbstractProperty(`type` = name, description = qName, format = format)
        case DataType.ContainerDataType(name, tpe, uniqueItems) =>
          AbstractProperty(`type` = name)
      }
    }

    def mkCollectionProperty(tpe: Type): Option[Property] = {
      val param = tpe.dealias.typeArgs.head
      val prop =
        if (param.isPrimitive)
          mkPrimitiveProperty(param).some
        else if (param.isCollection)
          typeToProp(param)
        else
          RefProperty(ref = param.simpleName).some 

      prop.map(p => ArrayProperty(items = p))
    }

    val schema = {
      try otpe.flatMap(typeToProp)
      catch { case NonFatal(t) =>
        logger.warn(t)(s"Failed to build model for type ${otpe.get}")
        None
      }
    }
    code -> Response(description = descr, schema = schema)
  }

  def mkQueryParam(rule: QueryCapture[_]): QueryParameter =
    QueryParameter(
      `type`       = getType(rule.m.tpe),
      name         = rule.name.some,
      required     = rule.default.isEmpty,
      defaultValue = rule.default.map(_.toString))

  def mkHeaderParam(key: HeaderKey.Extractable): HeaderParameter =
    HeaderParameter(
      `type`   = "string",
      name     = key.name.toString.some,
      required = true)

  def linearizeStack(stack: List[PathRule]): List[List[PathOperation]] = {

    def go(stack: List[PathRule], acc: List[PathOperation]): List[List[PathOperation]] =
      stack match {
        case PathOr(a, b)::xs           => go(a::xs, acc):::go(b::xs, acc)
        case PathAnd(a, b) :: xs        => go(a::b::xs, acc)
        case (m@ MetaCons(a, meta))::xs => go(a::xs, m::acc)
        case (op: PathOperation)::xs    => go(xs, op::acc)
        case Nil                        => acc::Nil
      }

    go(stack, Nil).map(_.reverse)
  }

  def addParamToPath(path: Path, param: Parameter): Path =
    path.copy(parameters = param :: path.parameters)

  def addOrDescriptions[A <: Parameter](set: (A, String) => A)(as: List[A], bs: List[A], tpe: String): List[A] =
    if (bs.isEmpty) as
    else if (as.isEmpty) bs
    else
      as.map(set(_, s"Optional if the following $tpe are satisfied: " + bs.flatMap(_.name).mkString("[",", ", "]")))

  def getType(m: Type): String =
    TypeBuilder.DataType(m).name
}
