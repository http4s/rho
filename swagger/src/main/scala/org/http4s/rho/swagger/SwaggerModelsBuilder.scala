package org.http4s
package rho
package swagger

import cats.syntax.option._
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.RequestAST._
import org.http4s.rho.bits._
import org.log4s.getLogger

import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

private[swagger] class SwaggerModelsBuilder(formats: SwaggerFormats) {
  import models._

  private[this] val logger = getLogger

  def mkSwagger[F[_]](info: Info, rr: RhoRoute[F, _])(s: Swagger)(implicit etag: WeakTypeTag[F[_]]): Swagger =
    Swagger(
      info        = info.some,
      paths       = collectPaths(rr)(s),
      definitions = collectDefinitions(rr)(s))

  def collectPaths[F[_]](rr: RhoRoute[F, _])(s: Swagger)(implicit etag: WeakTypeTag[F[_]]): Map[String, Path] = {
    val pairs = mkPathStrs(rr).map { ps =>
      val o = mkOperation(ps, rr)
      val p0 = s.paths.get(ps).getOrElse(Path())
      val p1 = rr.method.name.toLowerCase match {
        case "get"     => p0.copy(get = o.some)
        case "put"     => p0.copy(put = o.some)
        case "post"    => p0.copy(post = o.some)
        case "delete"  => p0.copy(delete = o.some)
        case "patch"   => p0.copy(patch = o.some)
        case "options" => p0.copy(options = o.some)
        case "head"    => p0.copy(head = o.some)
        case unknown   =>
          logger.warn("unrecognized method: " + unknown)
          p0
      }
      ps -> p1
    }
    pairs.foldLeft(s.paths) { case (paths, (s, p)) => paths.updated(s, p) }
  }

  def collectDefinitions[F[_]](rr: RhoRoute[F, _])(s: Swagger)(implicit etag: WeakTypeTag[F[_]]): Map[String, Model] = {
    val initial: Set[Model] = s.definitions.values.toSet
    (collectResultTypes(rr) ++ collectCodecTypes(rr) ++ collectQueryTypes(rr))
      .foldLeft(initial)((s, tpe) => s ++ TypeBuilder.collectModels(tpe, s, formats, etag.tpe))
      .map(m => m.id2 -> m)
      .toMap
  }

  def collectResultTypes[F[_]](rr: RhoRoute[F, _]): Set[Type] =
    rr.resultInfo.collect {
      case TypeOnly(tpe)         => tpe
      case StatusAndType(_, tpe) => tpe
    }

  def collectCodecTypes[F[_]](rr: RhoRoute[F, _]): Set[Type] =
    rr.router match {
      case r: CodecRouter[F, _, _] => Set(r.entityType)
      case _                    => Set.empty
    }

  def collectQueryTypes[F[_]](rr: RhoRoute[F, _]): Seq[Type] = {
    def go(stack: List[RequestRule[F]]): List[Type] =
      stack match {
        case Nil                                         => Nil
        case AndRule(a, b)::xs                           => go(a::b::xs)
        case OrRule(a, b)::xs                            => go(a::b::xs)
        case (EmptyRule() | CaptureRule(_))::xs          => go(xs)
        case MapRule(r, _)::xs                           => go(r::xs)
        case IgnoreRule(r)::xs                           => go(r::xs)
        case MetaRule(x, q@QueryMetaData(_,_,_,_,_))::xs =>
          val tpe = q.m.tpe
          TypeBuilder.DataType.fromType(tpe) match {
            case _ : TypeBuilder.DataType.ComplexDataType =>
              tpe :: go(x::xs)
            case TypeBuilder.DataType.ContainerDataType(_, Some(_: TypeBuilder.DataType.ComplexDataType), _) =>
              q.m.tpe.typeArgs.head :: go(x::xs)
            case _ => go(x::xs)
          }

        case MetaRule(x, _)::xs => go(x::xs)
      }

    go(rr.rules::Nil)
  }

  def mkPathStrs[F[_]](rr: RhoRoute[F, _]): List[String] = {

    def go(stack: List[PathOperation], pathStr: String): String =
      stack match {
        case Nil                          => if(pathStr.isEmpty) "/" else pathStr
        case PathMatch("") :: Nil         => pathStr + "/"
        case PathMatch("") :: xs          => go(xs, pathStr)
        case PathMatch(s) :: xs           => go(xs, pathStr + "/" + s)
        case MetaCons(_, _) :: xs         => go(xs, pathStr)
        case PathCapture(id, _, _, _)::xs => go(xs, s"$pathStr/{$id}")
        case CaptureTail :: _             => pathStr + "/{tail...}"
      }

    linearizeStack(rr.path::Nil).map(go(_, ""))
  }

  def collectPathParams[F[_]](rr: RhoRoute[F, _]): List[PathParameter] = {

    def go(stack: List[PathOperation], pps: List[PathParameter]): List[PathParameter] =
      stack match {
        case Nil                               => pps
        case PathMatch("") :: xs               => go(xs, pps)
        case PathMatch(_) :: xs                => go(xs, pps)
        case MetaCons(_, _) :: xs              => go(xs, pps)
        case PathCapture(id, desc, p, _) :: xs => go(xs, mkPathParam[F](id, desc, p.asInstanceOf[StringParser[F, String]])::pps)
        case CaptureTail :: _                  => PathParameter(`type` = "string", name = "tail...".some) :: Nil
      }

    linearizeStack(rr.path::Nil).flatMap(go(_, Nil)).reverse
  }

  def collectBodyParams[F[_]](rr: RhoRoute[F, _]): Option[BodyParameter] =
    rr.router match {
      case r: CodecRouter[_, _, _] => mkBodyParam(r).some
      case _                       => none
    }

  def collectResponses[F[_]](rr: RhoRoute[F, _])(implicit etag: WeakTypeTag[F[_]]): Map[String, Response] =
    rr.resultInfo.collect {
      case TypeOnly(tpe)         => mkResponse("200", "OK", tpe.some, etag.tpe)
      case StatusAndType(s, tpe) => mkResponse(s.code.toString, s.reason, tpe.some, etag.tpe)
      case StatusOnly(s)         => mkResponse(s.code.toString, s.reason, none, etag.tpe)
    }.toMap

  def collectSummary[F[_]](rr: RhoRoute[F, _]): Option[String] = {

    def go(stack: List[PathOperation], summary: Option[String]): Option[String] =
      stack match {
        case PathMatch("") :: Nil          => go(Nil, summary)
        case PathMatch(_) :: xs            => go(xs, summary)
        case PathCapture(_, _, _, _) :: xs => go(xs, summary)
        case CaptureTail :: _              => summary

        case MetaCons(_, meta)::xs =>
          meta match {
            case RouteDesc(meta) => meta.some
            case _               => go(xs, summary)
          }

        case Nil => summary
      }

    linearizeStack(rr.path::Nil).flatMap(go(_, None)).headOption
  }

  def collectSecurityScopes[F[_]](rr: RhoRoute[F, _]): List[Map[String, List[String]]] = {

    def go(stack: List[PathOperation]): Option[Map[String, List[String]]] =

      stack match {
        case Nil => None
        case MetaCons(_, RouteSecurityScope(secScope)) :: _ => secScope.some
        case _ :: xs => go(xs)
      }

     linearizeStack(rr.path::Nil).flatMap(go)
  }

  def collectOperationParams[F[_]](rr: RhoRoute[F, _]): List[Parameter] =
    collectPathParams(rr) ::: collectQueryParams(rr) ::: collectHeaderParams(rr) ::: collectBodyParams(rr).toList

  def collectQueryParams[F[_]](rr: RhoRoute[F, _]): List[Parameter] = {
    def go(stack: List[RequestRule[F]]): List[Parameter] =
      stack match {
        case AndRule(a, b)::xs => go(a::b::xs)
        case MapRule(r, _)::xs => go(r::xs)
        case (EmptyRule() | CaptureRule(_))::xs => go(xs)

        case OrRule(a, b)::xs =>
          val as = go(a::xs)
          val bs = go(b::xs)
          val set: (Parameter, String) => Parameter =
            (p, s) => p.withDesc(p.description.map(_ + s).orElse(s.some))

          addOrDescriptions(set)(as, bs, "params") :::
          addOrDescriptions(set)(bs, as, "params")

        case MetaRule(rs, q@QueryMetaData(_,_,_,_,_))::xs => mkQueryParam[F](q.asInstanceOf[QueryMetaData[F, _]])::go(rs::xs)

        case MetaRule(rs, m: TextMetaData)::xs =>
          go(rs::Nil).map(_.withDesc(m.msg.some)) ::: go(xs)

        case MetaRule(a, _)::xs => go(a::xs)

        case IgnoreRule(r)::xs => go(r::xs)

        case Nil => Nil
      }

    go(rr.rules::Nil)
  }

  def collectHeaderParams[F[_]](rr: RhoRoute[F, _]): List[HeaderParameter] = {
    def go(stack: List[RequestRule[F]]): List[HeaderParameter] =
      stack match {
        case Nil                                   => Nil
        case AndRule(a,b)::xs                      => go(a::b::xs)
        case MetaRule(a,HeaderMetaData(key,d))::xs => mkHeaderParam(key, d)::go(a::xs)
        case MetaRule(a,_)::xs                     => go(a::xs)
        case (EmptyRule() | CaptureRule(_))::xs    => go(xs)
        case MapRule(r,_)::xs                      => go(r::xs)
        case IgnoreRule(r)::xs                     => go(r::xs)
        case OrRule(a, b)::xs =>
          val as = go(a::xs)
          val bs = go(b::xs)
          val set: (HeaderParameter, String) => HeaderParameter =
            (p, s) => p.copy(description = p.description.map(_ + s).orElse(s.some))
          addOrDescriptions(set)(as, bs, "headers") :::
          addOrDescriptions(set)(bs, as, "headers")
      }

    go(rr.rules::Nil)
  }

  def mkOperation[F[_]](pathStr: String, rr: RhoRoute[F, _])(implicit etag: WeakTypeTag[F[_]]): Operation = {
    val parameters = collectOperationParams(rr)

    Operation(
      tags        = pathStr.split("/").filterNot(_ == "").headOption.getOrElse("/") :: Nil,
      summary     = collectSummary(rr),
      consumes    = rr.validMedia.toList.map(_.renderString),
      produces    = rr.responseEncodings.toList.map(_.renderString),
      operationId = mkOperationId(pathStr, rr.method, parameters).some,
      parameters  = parameters,
      security    = collectSecurityScopes(rr),
      responses   = collectResponses(rr))
  }

  def mkOperationId(path: String, method: Method, parameters: List[Parameter]): String = {
    val showParameters =
      if (parameters.isEmpty) ""
      else parameters.flatMap(_.name).mkString("-", "-", "")

    method.toString.toLowerCase +
      path.split("/")
        .filter(s => !s.isEmpty && !(s.startsWith("{") && s.endsWith("}")))
        .map(_.capitalize)
        .mkString +
      showParameters
  }

  def mkBodyParam[F[_]](r: CodecRouter[F, _, _]): BodyParameter = {
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

  def mkPathParam[F[_]](name: String, description: Option[String], parser: StringParser[F, String]): PathParameter = {
    val tpe = parser.typeTag.map(tag => getType(tag.tpe)).getOrElse("string")
    PathParameter(`type` = tpe, name = name.some, description = description, required = true)
  }

  def mkResponse(code: String, descr: String, otpe: Option[Type], fType: Type): (String, Response) = {

    def typeToProp(tpe: Type): Option[Property] =
      if (Reflector.isExcluded(tpe))
        None
      else if (tpe.isUnitOrVoid)
        None
      else if (tpe.isPrimitive)
        mkPrimitiveProperty(tpe).some
      else if (tpe.isMap)
        mkMapProperty(tpe)
      else if (tpe.isCollection)
        mkCollectionProperty(tpe)
      else if (tpe.isStream)
        typeToProp(tpe.dealias.typeArgs(1))
      else if (tpe.isEffect(fType))
        typeToProp(tpe.dealias.typeArgs(0))
      else if (tpe.isSwaggerFile)
        AbstractProperty(`type` = "file").some
      else
        RefProperty(ref = tpe.simpleName).some

    def mkPrimitiveProperty(tpe: Type): Property = {
      import TypeBuilder._

      DataType.fromType(tpe) match {
        case DataType.ValueDataType(name, format, qName) =>
          AbstractProperty(`type` = name, description = qName, format = format)
        case DataType.ComplexDataType(name, qName) =>
          AbstractProperty(`type` = name, description = qName)
        case DataType.ContainerDataType(name, _, _) =>
          AbstractProperty(`type` = name)
        case DataType.EnumDataType(enums) =>
          StringProperty(enums = enums)
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

    def mkMapProperty(tpe: Type): Option[Property] = {
      val param = tpe.dealias.typeArgs.last
      val prop =
        if (param.isPrimitive)
          mkPrimitiveProperty(param).some
        else if (param.isCollection)
          typeToProp(param)
        else
          RefProperty(ref = param.simpleName).some

      prop.map(p => MapProperty(additionalProperties = p))
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

  def mkQueryParam[F[_]](rule: QueryMetaData[F, _]): Parameter = {
    val required = !(rule.m.tpe.isOption || rule.default.isDefined)

    TypeBuilder.DataType(rule.m.tpe) match {
      case TypeBuilder.DataType.ComplexDataType(nm, _) =>
        QueryParameter(
          `type`       = nm.some,
          name         = rule.name.some,
          description  = rule.description,
          required     = required,
          defaultValue = rule.default.map(_ => "") // TODO ideally need to use the parser to serialize it into string
        )
      // XXX uniqueItems is indeed part of `parameter` api,
      // see here: http://swagger.io/specification/#parameterObject
      // however the java api does not include it...
      case TypeBuilder.DataType.ContainerDataType(_, dt, _) =>
        val itemTpe = dt match {
          case Some(TypeBuilder.DataType.ComplexDataType(nm, _)) =>
            models.AbstractProperty(`type` = nm).some
          // XXX need to revisit to take care of recursive array type
          case Some(tpe: TypeBuilder.DataType) =>
            models.AbstractProperty(tpe.name).some
          case None => None
        }

        QueryParameter(
          name         = rule.name.some,
          items        = itemTpe,
          required     = required,
          description  = rule.description,
          defaultValue = rule.default.map(_ => ""), // TODO ideally need to put something like [...] here
          isArray      = true
        )

      case TypeBuilder.DataType.ValueDataType(nm, _, _) =>
        QueryParameter(
          `type`       = nm.some,
          name         = rule.name.some,
          description  = rule.description,
          required     = required,
          defaultValue = rule.default.map(_.toString)
        )

      case TypeBuilder.DataType.EnumDataType(enums) =>
        QueryParameter(
          `type`       = "string".some,
          name         = rule.name.some,
          description  = rule.description,
          required     = required,
          defaultValue = rule.default.map(_.toString),
          enums        = enums.toList
        )
    }
  }

  def mkHeaderParam(key: HeaderKey.Extractable, hasDefault: Boolean): HeaderParameter =
    HeaderParameter(
      `type`   = "string",
      name     = key.name.toString.some,
      required = !hasDefault)

  def linearizeStack(stack: List[PathRule]): List[List[PathOperation]] = {

    def go(stack: List[PathRule], acc: List[PathOperation]): List[List[PathOperation]] =
      stack match {
        case PathOr(a, b) :: xs         => go(a::xs, acc):::go(b::xs, acc)
        case PathAnd(a, b) :: xs        => go(a::b::xs, acc)
        case (m@ MetaCons(a, _)) :: xs  => go(a::xs, m::acc)
        case (op: PathOperation) :: xs  => go(xs, op::acc)
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

  def getType(m: Type): String = {
    TypeBuilder.DataType(m).name
  }
}
