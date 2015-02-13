package org.http4s.rho
package swagger

import com.wordnik.swagger.models._
import com.wordnik.swagger.models.parameters._
import com.wordnik.swagger.models.properties._

import scala.collection.JavaConversions._

import scalaz._, Scalaz._

trait SwaggerModelsInstances {

  // I wonder if all this boilerplate could be avoided?

  //
  // Parameters
  //

  implicit val parameterInstances: Equal[Parameter] with Show[Parameter] =
    new Equal[Parameter] with Show[Parameter] {
      def equal(p1: Parameter, p2: Parameter): Boolean =
        (p1, p2) match {
          case (p1: QueryParameter,  p2: QueryParameter)  => p1 === p2
          case (p1: PathParameter,   p2: PathParameter)   => p1 === p2
          case (p1: HeaderParameter, p2: HeaderParameter) => p1 === p2
          case (p1: BodyParameter,   p2: BodyParameter)   => p1 === p2
          case (p1: CookieParameter, p2: CookieParameter) => p1 === p2
          case (p1: FormParameter,   p2: FormParameter)   => p1 === p2
          case (p1: RefParameter,    p2: RefParameter)    => p1 === p2
          case _                                          => false
        }              
      override def shows(p: Parameter): String =
        p match {
          case p: QueryParameter  => p.shows
          case p: PathParameter   => p.shows
          case p: HeaderParameter => p.shows
          case p: BodyParameter   => p.shows
          case p: CookieParameter => p.shows
          case p: FormParameter   => p.shows
          case p: RefParameter    => p.shows
          case _                  =>
            List(p.getName, p.getIn,p.getDescription, p.getRequired.shows)
              .mkString("Parameter(", ",", ")")
        }
    }

  implicit val queryParameterInstances: Equal[QueryParameter] with Show[QueryParameter] =
    new Equal[QueryParameter] with Show[QueryParameter] {
      def equal(qp1: QueryParameter, qp2: QueryParameter): Boolean = {
        qp1.getName         === qp2.getName         &&
        qp1.getIn           === qp2.getIn           &&
        qp1.getDescription  === qp2.getDescription  &&
        qp1.getRequired     === qp2.getRequired     &&
        qp1.getType         === qp2.getType         &&
        qp1.getDefaultValue === qp2.getDefaultValue
      }
      override def shows(qp: QueryParameter): String =
        List(qp.getName, qp.getIn, qp.getDescription, qp.getRequired.shows,
          qp.getType, qp.getDefaultValue).mkString("QueryParameter(", ",", ")")      
    }

  implicit val headerParameterInstances: Equal[HeaderParameter] with Show[HeaderParameter] =
    new Equal[HeaderParameter] with Show[HeaderParameter] {
      def equal(hp1: HeaderParameter, hp2: HeaderParameter): Boolean = {
        hp1.getName         === hp2.getName         &&
        hp1.getIn           === hp2.getIn           &&
        hp1.getDescription  === hp2.getDescription  &&
        hp1.getRequired     === hp2.getRequired     &&
        hp1.getType         === hp2.getType         &&
        hp1.getDefaultValue === hp2.getDefaultValue
      }
      override def shows(hp: HeaderParameter): String =
        List(hp.getName, hp.getIn, hp.getDescription, hp.getRequired.shows,
          hp.getType, hp.getDefaultValue).mkString("HeaderParameter(", ",", ")")
    }

  implicit val pathParameterInstances: Equal[PathParameter] with Show[PathParameter] =
    new Equal[PathParameter] with Show[PathParameter]{
      def equal(pp1: PathParameter, pp2: PathParameter): Boolean = {
        pp1.getName         === pp2.getName         &&
        pp1.getIn           === pp2.getIn           &&
        pp1.getDescription  === pp2.getDescription  &&
        pp1.getRequired     === pp2.getRequired     &&
        pp1.getType         === pp2.getType         &&
        pp1.getDefaultValue === pp2.getDefaultValue
      }
      override def shows(pp: PathParameter): String =
        List(pp.getName, pp.getIn, pp.getDescription, pp.getRequired.shows,
          pp.getType, pp.getDefaultValue).mkString("PathParameter(", ",", ")")
    }

  implicit val bodyParameterInstances: Equal[BodyParameter] with Show[BodyParameter] =
    new Equal[BodyParameter] with Show[BodyParameter] {
      def equal(bp1: BodyParameter, bp2: BodyParameter): Boolean = {
        true
      }
      override def shows(bp: BodyParameter): String =
        List().mkString("BodyParameter(", ",", ")")
    }

  implicit val cookieParameterInstances: Equal[CookieParameter] with Show[CookieParameter] =
    new Equal[CookieParameter] with Show[CookieParameter] {
      def equal(cp1: CookieParameter, cp2: CookieParameter): Boolean = {
        true
      }
      override def shows(cp: CookieParameter): String =
        List().mkString("CookieParameter(", ",", ")")
    }

  
  implicit val refParameterInstances: Equal[RefParameter] with Show[RefParameter] =
    new Equal[RefParameter] with Show[RefParameter] {
      def equal(rp1: RefParameter, rp2: RefParameter): Boolean = {
        true
      }
      override def shows(rp: RefParameter): String =
        List().mkString("RefParameter(", ",", ")")
    }

  implicit val formParameterInstances: Equal[FormParameter] with Show[FormParameter] =
    new Equal[FormParameter] with Show[FormParameter] {
      def equal(fp1: FormParameter, fp2: FormParameter): Boolean = {
        true
      }
      override def shows(fp: FormParameter): String =
        List().mkString("FormParameter(", ",", ")")
    }

  //
  // Operation
  //

  implicit val operationInstances: Equal[Operation] with Show[Operation] =
    new Equal[Operation] with Show[Operation] {
      def equal(o1: Operation, o2: Operation): Boolean = {
        if (o1 eq o2) true
        else if ((o1 eq null) || (o2 eq null)) false
        else {
          o1.getTags        === o2.getTags        &&
          o1.getSummary     === o2.getSummary     &&
          o1.getDescription === o2.getDescription &&
          o1.getOperationId === o2.getOperationId &&
          //o1.getSchemes     === o2.getSchemes     &&
          o1.getConsumes    === o2.getConsumes    &&
          o1.getProduces    === o2.getProduces    &&
          o1.getParameters  === o2.getParameters  &&
          // Map<String, Response> responses
          // List<Map<String, List<String>>> security
          // ExternalDocs externalDocs
          o1.isDeprecated === o2.isDeprecated
        }
      }
      override def shows(o: Operation): String = {
        if (o eq null) "null"
        else
          List(o.getTags.shows, o.getSummary.shows, o.getDescription.shows,
            o.getOperationId.shows, o.getConsumes.shows, o.getProduces.shows,
            o.getParameters.shows, o.isDeprecated.shows).mkString("Operation(", ",", ")")
      }
    }

  //
  // Path
  //

  implicit val pathIntances: Equal[Path] with Show[Path] =
    new Equal[Path] with Show[Path] {
      def equal(p1: Path, p2: Path): Boolean = {
        if (p1 eq p2) true
        else if ((p1 eq null) || (p2 eq null)) false
        else {
          p1.getGet        === p2.getGet        &&
          p1.getPut        === p2.getPut        &&
          p1.getPost       === p2.getPost       &&
          p1.getDelete     === p2.getDelete     &&
          p1.getPatch      === p2.getPatch      &&
          p1.getOptions    === p2.getOptions    &&
          p1.getParameters === p2.getParameters
          //Map<String, Object> vendorExtensions
        }
      }
      override def shows(p: Path): String =
        List(p.getGet.shows, p.getPut.shows, p.getPost.shows,
          p.getDelete.shows, p.getPatch.shows, p.getOptions.shows,
          p.getParameters.shows).mkString("Path(", ",", ")")
    }

  //
  // Response
  //

  implicit val responseInstances: Equal[Response] with Show[Response] =
    new Equal[Response] with Show[Response] {
      def equal(r1: Response, r2: Response): Boolean = {
        if (r1 eq r2) true
        else if ((r1 eq null) || (r2 eq null)) false
        else {
          r1.getDescription === r2.getDescription
          //Property schema;
          //Map<String, String> examples;
          //Map<String, Property> headers;
        }
      }
      override def shows(r: Response): String = {
        if (r eq null) "null"
        else
          List(r.getDescription).mkString("Response(", ",", ")")
      }
    }

  //
  // Java
  //

  implicit def javaListInstances[A:Equal:Show]: Equal[java.util.List[A]] with Show[java.util.List[A]] =
    new Equal[java.util.List[A]] with Show[java.util.List[A]] {
      def equal(l1: java.util.List[A], l2: java.util.List[A]): Boolean =
        if (l1 eq l2) true
        else if ((l1 eq null) || (l2 eq null)) false
        else std.list.listEqual[A].equal(l1.toList, l2.toList)
      override def shows(l: java.util.List[A]): String =
        if (l eq null) "null"
        else std.list.listShow[A].shows(l.toList)
    }

  implicit val javaStringInstances: Equal[java.lang.String] with Show[java.lang.String] =
    new Equal[java.lang.String] with Show[java.lang.String] {
      def equal(s1: java.lang.String, s2: java.lang.String): Boolean =
        s1 == s2
      override def shows(s: java.lang.String): String =
        if (s eq null) "null" else s
    }

  implicit val javaBooleanInstances0: Equal[Boolean] with Show[Boolean] =
    new Equal[Boolean] with Show[Boolean] {
      def equal(b1: Boolean, b2: Boolean): Boolean = b1 == b2
      override def shows(b: Boolean): String = b.toString
    }

  implicit val javaBooleanInstances1: Equal[java.lang.Boolean] with Show[java.lang.Boolean] =
    new Equal[java.lang.Boolean] with Show[java.lang.Boolean] {
      def equal(b1: java.lang.Boolean, b2: java.lang.Boolean): Boolean =
        b1 == b2
      override def shows(b: java.lang.Boolean): String =
        if (b eq null) "null" else b.toString
    }
}
