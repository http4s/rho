package org.http4s
package rho
package swagger

import cats.syntax.option._
import cats.syntax.show._
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.RequestAST._
import org.http4s.rho.bits._
import org.log4s.getLogger

import scala.collection.immutable.ListMap
import scala.collection.immutable.Seq
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal
import org.typelevel.ci.CIString

private[swagger] class SwaggerModelsBuilder[F[_]](formats: SwaggerFormats)(implicit
    st: ShowType,
    etag: WeakTypeTag[F[_]]) {
  import models._

  private[this] val logger = getLogger

  def mkSwagger(rr: RhoRoute[F, _])(s: Swagger): Swagger =
    s.copy(paths = collectPaths(rr)(s), definitions = collectDefinitions(rr)(s))

  def collectPaths(rr: RhoRoute[F, _])(s: Swagger): ListMap[String, Path] = {
    val pairs = linearizeRoute(rr).map { lr =>
      val o = mkOperation(lr)
      val p0 = s.paths.getOrElse(lr.pathString, Path())
      val p1 = lr.method.name.toLowerCase match {
        case "get" => p0.copy(get = o.some)
        case "put" => p0.copy(put = o.some)
        case "post" => p0.copy(post = o.some)
        case "delete" => p0.copy(delete = o.some)
        case "patch" => p0.copy(patch = o.some)
        case "options" => p0.copy(options = o.some)
        case "head" => p0.copy(head = o.some)
        case unknown =>
          logger.warn("unrecognized method: " + unknown)
          p0
      }
      lr.pathString -> p1
    }
    pairs.foldLeft(s.paths) { case (paths, (s, p)) => paths.updated(s, p) }
  }

  def collectDefinitions(rr: RhoRoute[F, _])(s: Swagger): Map[String, Model] = {
    val initial: Set[Model] = s.definitions.values.toSet
    (collectResultTypes(rr) ++ collectCodecTypes(rr) ++ collectQueryTypes(rr))
      .foldLeft(initial)((s, tpe) => s ++ TypeBuilder.collectModels(tpe, s, formats, etag.tpe))
      .map(m => m.id2 -> m)
      .toMap
  }

  def collectResultTypes(rr: RhoRoute[F, _]): Set[Type] =
    rr.resultInfo.collect {
      case TypeOnly(tpe) => tpe
      case StatusAndType(_, tpe) => tpe
    }

  def collectCodecTypes(rr: RhoRoute[F, _]): Set[Type] =
    rr.router match {
      case r: CodecRouter[F, _, _] => Set(r.entityType)
      case _ => Set.empty
    }

  def collectQueryTypes(rr: RhoRoute[F, _]): Seq[Type] = {
    def go(stack: List[RequestRule[F]]): List[Type] =
      stack match {
        case Nil => Nil
        case AndRule(a, b) :: xs => go(a :: b :: xs)
        case OrRule(a, b) :: xs => go(a :: b :: xs)
        case (EmptyRule() | CaptureRule(_)) :: xs => go(xs)
        case MapRule(r, _) :: xs => go(r :: xs)
        case IgnoreRule(r) :: xs => go(r :: xs)
        case MetaRule(x, q @ QueryMetaData(_, _, _, _, _, _)) :: xs =>
          val tpe = q.m.tpe
          TypeBuilder.DataType.fromType(tpe) match {
            case _: TypeBuilder.DataType.ComplexDataType =>
              tpe :: go(x :: xs)
            case TypeBuilder.DataType.ContainerDataType(
                  _,
                  Some(_: TypeBuilder.DataType.ComplexDataType),
                  _
                ) =>
              q.m.tpe.dealias.typeArgs.head :: go(x :: xs)
            case _ => go(x :: xs)
          }

        case MetaRule(x, _) :: xs => go(x :: xs)
      }

    go(rr.rules :: Nil)
  }

  def collectPathParams(lr: LinearRoute): List[PathParameter] = {

    def go(stack: List[PathOperation], pps: List[PathParameter]): List[PathParameter] =
      stack match {
        case Nil => pps
        case PathMatch.empty :: xs => go(xs, pps)
        case PathMatch(_) :: xs => go(xs, pps)
        case MetaCons(_, _) :: xs => go(xs, pps)
        case PathCapture(id, desc, p, _) :: xs =>
          go(xs, mkPathParam(id, desc, p.asInstanceOf[StringParser[F, String]]) :: pps)
        case CaptureTail :: _ => PathParameter(`type` = "string", name = "tail...".some) :: Nil
      }

    go(lr.path, Nil).reverse
  }

  def collectBodyParams(lr: LinearRoute): Option[BodyParameter] =
    lr.entityType.map(mkBodyParam)

  def collectResponses(lr: LinearRoute): Map[String, Response] =
    lr.resultInfo.collect {
      case TypeOnly(tpe) => mkResponse("200", "OK", tpe.some, etag.tpe)
      case StatusAndType(s, tpe) => mkResponse(s.code.toString, s.reason, tpe.some, etag.tpe)
      case StatusOnly(s) => mkResponse(s.code.toString, s.reason, none, etag.tpe)
    }.toMap

  def collectSummary(lr: LinearRoute): Option[String] = {

    def go(stack: List[PathOperation], summary: Option[String]): Option[String] =
      stack match {
        case PathMatch.empty :: Nil => go(Nil, summary)
        case PathMatch(_) :: xs => go(xs, summary)
        case PathCapture(_, _, _, _) :: xs => go(xs, summary)
        case CaptureTail :: _ => summary

        case MetaCons(_, meta) :: xs =>
          meta match {
            case RouteDesc(meta) => meta.some
            case _ => go(xs, summary)
          }

        case Nil => summary
      }

    go(lr.path, None)
  }

  def collectTags(lr: LinearRoute): List[String] = {

    def go(stack: List[PathOperation], tags: List[String]): List[String] =
      stack match {
        case PathMatch.empty :: xs => go(xs, tags)
        case PathMatch(segment) :: xs =>
          tags match {
            case Nil => go(xs, segment.decoded() :: Nil)
            case ts => go(xs, ts)
          }
        case PathCapture(id, _, _, _) :: xs =>
          tags match {
            case Nil => go(xs, id :: Nil)
            case ts => go(xs, ts)
          }
        case Nil | CaptureTail :: _ =>
          tags match {
            case Nil => "/" :: Nil
            case ts => ts
          }
        case MetaCons(_, meta) :: xs =>
          meta match {
            case RouteTags(ts) => ts
            case _ => go(xs, tags)
          }
      }

    go(lr.path, Nil)
  }

  def collectSecurityScopes(lr: LinearRoute): List[Map[String, List[String]]] = {

    def go(stack: List[PathOperation]): Option[Map[String, List[String]]] =
      stack match {
        case Nil => None
        case MetaCons(_, RouteSecurityScope(secScope)) :: _ => secScope.some
        case _ :: xs => go(xs)
      }

    go(lr.path).toList
  }

  def collectOperationParams(lr: LinearRoute): List[Parameter] =
    collectPathParams(lr) ::: collectQueryParams(lr) ::: collectHeaderParams(
      lr
    ) ::: collectBodyParams(lr).toList

  def collectQueryParams(lr: LinearRoute): List[Parameter] = {
    def go(stack: List[RequestRule[F]]): List[Parameter] =
      stack match {
        case AndRule(a, b) :: xs => go(a :: b :: xs)
        case MapRule(r, _) :: xs => go(r :: xs)
        case (EmptyRule() | CaptureRule(_)) :: xs => go(xs)

        case OrRule(a, b) :: xs =>
          val as = go(a :: xs)
          val bs = go(b :: xs)
          val set: (Parameter, String) => Parameter =
            (p, s) => p.withDesc(p.description.map(_ + s).orElse(s.some))

          addOrDescriptions(set)(as, bs, "params") :::
            addOrDescriptions(set)(bs, as, "params")

        case MetaRule(rs, q @ QueryMetaData(_, _, _, _, _, _)) :: xs =>
          mkQueryParam(q.asInstanceOf[QueryMetaData[F, _]]) :: go(rs :: xs)

        case MetaRule(rs, m: TextMetaData) :: xs =>
          go(rs :: Nil).map(_.withDesc(m.msg.some)) ::: go(xs)

        case MetaRule(a, _) :: xs => go(a :: xs)

        case IgnoreRule(r) :: xs => go(r :: xs)

        case Nil => Nil
      }

    go(lr.rules :: Nil)
  }

  def collectHeaderParams(lr: LinearRoute): List[HeaderParameter] = {
    def go(stack: List[RequestRule[F]]): List[HeaderParameter] =
      stack match {
        case Nil => Nil
        case AndRule(a, b) :: xs => go(a :: b :: xs)
        case MetaRule(a, HeaderMetaData(key, r)) :: xs => mkHeaderParam(key, r) :: go(a :: xs)
        case MetaRule(a, _) :: xs => go(a :: xs)
        case (EmptyRule() | CaptureRule(_)) :: xs => go(xs)
        case MapRule(r, _) :: xs => go(r :: xs)
        case IgnoreRule(r) :: xs => go(r :: xs)
        case OrRule(a, b) :: xs =>
          val as = go(a :: xs)
          val bs = go(b :: xs)
          val set: (HeaderParameter, String) => HeaderParameter =
            (p, s) => p.copy(description = p.description.map(_ + s).orElse(s.some))
          addOrDescriptions(set)(as, bs, "headers") :::
            addOrDescriptions(set)(bs, as, "headers")
      }

    go(lr.rules :: Nil)
  }

  def renderMediaRange: MediaRange => String = {
    case tpe: MediaType => tpe.show
    case range: MediaRange => range.show
  }

  def mkOperation(lr: LinearRoute): Operation = {
    val parameters = collectOperationParams(lr)

    Operation(
      tags = collectTags(lr),
      summary = collectSummary(lr),
      consumes = lr.validMedia.toList.map(renderMediaRange),
      produces = lr.responseEncodings.toList.map(_.show),
      operationId = mkOperationId(lr, parameters).some,
      parameters = parameters,
      security = collectSecurityScopes(lr),
      responses = collectResponses(lr)
    )
  }

  def mkOperationId(lr: LinearRoute, parameters: List[Parameter]): String = {
    val showParameters =
      if (parameters.isEmpty) ""
      else parameters.flatMap(_.name).mkString("-", "-", "")

    lr.method.toString.toLowerCase +
      lr.pathString
        .split("/")
        .filter(s => !s.isEmpty && !(s.startsWith("{") && s.endsWith("}")))
        .map(_.capitalize)
        .mkString +
      showParameters
  }

  def mkBodyParam(entityType: Type): BodyParameter = {
    val tpe = entityType
    val model = if (tpe.isPrimitive) {
      val name = TypeBuilder.DataType(tpe).name
      ModelImpl(id = tpe.fullName, id2 = name, `type` = name.some, isSimple = true)
    } else if (tpe.isCollection) {
      val pType = tpe.dealias.typeArgs.head
      ArrayModel(
        id = tpe.fullName,
        id2 = tpe.fullName,
        `type` = "array".some,
        items = RefProperty(title = pType.simpleName.some, ref = pType.simpleName).some
      )
    } else RefModel(tpe.fullName, tpe.fullName, tpe.simpleName)
    BodyParameter(
      schema = model.some,
      name = "body".some,
      description = tpe.simpleName.some,
      required = !tpe.isOption
    )
  }

  def mkPathParam(
      name: String,
      description: Option[String],
      parser: StringParser[F, String]): PathParameter = {
    val tpe = parser.typeTag.map(tag => getType(tag.tpe)).getOrElse("string")
    PathParameter(`type` = tpe, name = name.some, description = description, required = true)
  }

  def mkResponse(
      code: String,
      descr: String,
      otpe: Option[Type],
      fType: Type): (String, Response) = {

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
      catch {
        case NonFatal(t) =>
          logger.warn(t)(s"Failed to build model for type ${otpe.get}")
          None
      }
    }
    code -> Response(description = descr, schema = schema)
  }

  def mkQueryParam(rule: QueryMetaData[F, _]): Parameter = {
    val required = !(rule.m.tpe.isOption || rule.default.isDefined)

    val tpe = if (rule.m.tpe.isOption) rule.m.tpe.dealias.typeArgs.head else rule.m.tpe
    TypeBuilder.DataType(tpe) match {
      case TypeBuilder.DataType.ComplexDataType(nm, _) =>
        QueryParameter(
          `type` = nm.some,
          name = rule.name.some,
          description = rule.description,
          required = required,
          defaultValue = rule.default.map(_ =>
            ""
          ) // TODO ideally need to use the parser to serialize it into string
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
          name = rule.name.some,
          items = itemTpe,
          required = required,
          description = rule.description,
          defaultValue =
            rule.default.map(_ => ""), // TODO ideally need to put something like [...] here
          isArray = true
        )

      case TypeBuilder.DataType.ValueDataType(nm, _, _) =>
        QueryParameter(
          `type` = nm.some,
          name = rule.name.some,
          description = rule.description,
          required = required,
          defaultValue = rule.default.map(_.toString)
        )

      case TypeBuilder.DataType.EnumDataType(enums) =>
        QueryParameter(
          `type` = "string".some,
          name = rule.name.some,
          description = rule.description,
          required = required,
          defaultValue = rule.default.map(_.toString),
          enums = if (rule.enums.nonEmpty) rule.enums.toList else enums.toList
        )
    }
  }

  def mkHeaderParam(key: CIString, isRequired: Boolean): HeaderParameter =
    HeaderParameter(`type` = "string", name = key.toString.some, required = isRequired)

  def linearizeRoute(rr: RhoRoute[F, _]): List[LinearRoute] = {

    def go(stack: List[PathRule], acc: List[PathOperation]): List[List[PathOperation]] =
      stack match {
        case PathOr(a, b) :: xs => go(a :: xs, acc) ::: go(b :: xs, acc)
        case PathAnd(a, b) :: xs => go(a :: b :: xs, acc)
        case (m @ MetaCons(a, _)) :: xs => go(a :: xs, m :: acc)
        case (op: PathOperation) :: xs => go(xs, op :: acc)
        case Nil => acc :: Nil
      }

    go(rr.path :: Nil, Nil)
      .map(_.reverse)
      .map(linearPath => LinearRoute(linearPath, rr))
  }

  def addParamToPath(path: Path, param: Parameter): Path =
    path.copy(parameters = param :: path.parameters)

  def addOrDescriptions[A <: Parameter](
      set: (A, String) => A)(as: List[A], bs: List[A], tpe: String): List[A] =
    if (bs.isEmpty) as
    else if (as.isEmpty) bs
    else
      as.map(
        set(
          _,
          s"Optional if the following $tpe are satisfied: " + bs
            .flatMap(_.name)
            .mkString("[", ", ", "]")
        )
      )

  def getType(m: Type): String =
    TypeBuilder.DataType(m).name

  case class LinearRoute(
      method: Method,
      path: List[PathOperation],
      rules: RequestRule[F],
      responseEncodings: Set[MediaType],
      resultInfo: Set[ResultInfo],
      validMedia: Set[MediaRange],
      entityType: Option[Type]) {

    lazy val pathString: String = {
      def go(stack: List[PathOperation], pathStr: String): String =
        stack match {
          case Nil => if (pathStr.isEmpty) "/" else pathStr
          case PathMatch.empty :: Nil => pathStr + "/"
          case PathMatch.empty :: xs => go(xs, pathStr)
          case PathMatch(s) :: xs => go(xs, pathStr + "/" + s)
          case MetaCons(_, _) :: xs => go(xs, pathStr)
          case PathCapture(id, _, _, _) :: xs => go(xs, s"$pathStr/{$id}")
          case CaptureTail :: _ => pathStr + "/{tail...}"
        }

      go(path, "")
    }
  }

  object LinearRoute {
    def apply(path: List[PathOperation], rr: RhoRoute[F, _]): LinearRoute = {
      val entityType = rr.router match {
        case r: CodecRouter[F, _, _] => r.entityType.some
        case _ => none
      }
      new LinearRoute(
        rr.method,
        path,
        rr.rules,
        rr.responseEncodings,
        rr.resultInfo,
        rr.validMedia,
        entityType
      )
    }
  }
}
