package org.http4s.rho
package swagger

import org.http4s._
import org.http4s.Method._
import org.http4s.Header._
import org.http4s.rho.bits.QueryAST.EmptyQuery

import org.specs2.scalaz.{Spec, ScalazMatchers}

import com.wordnik.swagger.{ models => m }
import com.wordnik.swagger.models.parameters.{PathParameter, QueryParameter, HeaderParameter, Parameter}

package object dummy {
  sealed abstract class Renderable
  case class ModelA(name: String, color: Int) extends Renderable
  case class ModelB(name: String, id: Long) extends Renderable
  case class ModelC(name: String, shape: String) extends Renderable
}

class ApiBuilderSpec extends Spec with ScalazMatchers with SwaggerModelsInstances {

  val swagger = new m.Swagger
  val builder = new ApiBuilder("0.1", swagger, DefaultSwaggerFormats)

  "ApiBuilder.analyzeQuery" should {
    "Analyze a simple query" in {
      val q = param[Int]("id")
      val qp = (new QueryParameter).name("id").required(true)
      qp.setType("integer")      
      builder.analyzeQuery(q.rule) must equal(List(qp))
    }

    "Analyze a simple query with a default value" in {
      val q = param[Int]("id", 6)
      val qp = (new QueryParameter).name("id").required(false)
      qp.setType("integer")
      qp.setDefaultValue(6.toString)
      builder.analyzeQuery(q.rule) must equal(List(qp))
    }

    "Analyze a query with two params" in {
      val q = param[Int]("id") & param[String]("str", "Hello")
      val qp1 = (new QueryParameter).name("id").required(true)
      val qp2 = (new QueryParameter).name("str").required(false)
      qp1.setType("integer")
      qp2.setType("string")
      qp2.setDefaultValue("Hello")
      builder.analyzeQuery(q.rule) must equal(List(qp1, qp2))
    }

    "Deal with Query OR structure" in {

      def orStr(str: String) = s"Optional if the following params are satisfied: [$str]"

      val q = param[Int]("id") || param[Int]("id2")
      val qp1 = (new QueryParameter).name("id").required(true).description(orStr("id2"))
      val qp2 = (new QueryParameter).name("id2").required(true).description(orStr("id"))
      qp1.setType("integer")
      qp2.setType("integer")

      builder.analyzeQuery(q.rule) must equal(List(qp1, qp2))
    }
  }

  "ApiBuilder.analyzeHeaders" should {
    "Analyze a single HeaderRule" in {
      val h = require(`Content-Length`)
      val hp = (new HeaderParameter).name("Content-Length")
      hp.setType("string")
      hp.setRequired(true)

      builder.analyzeHeaders(h.rule) must equal(List(hp))
    }

    "Analyze a two HeaderRules" in {
      val h = require(`Content-Length`) && require(`Content-MD5`)
      val hp1 = (new HeaderParameter).name("Content-Length")
      val hp2 = (new HeaderParameter).name("Content-MD5")
      hp1.setType("string")
      hp1.setRequired(true)
      hp2.setType("string")
      hp2.setRequired(true)

      builder.analyzeHeaders(h.rule) must equal(List(hp1, hp2))
    }

    "Analyze OR HeaderRules" in {
      def orStr(str: String) = s"Optional if the following headers are satisfied: [$str]"

      val h = require(`Content-Length`) || require(`Content-MD5`)
      val hp1 = (new HeaderParameter).name("Content-Length").description(orStr("Content-MD5"))
      val hp2 = (new HeaderParameter).name("Content-MD5").description(orStr("Content-Length"))
      hp1.setType("string")
      hp1.setRequired(true)
      hp2.setType("string")
      hp2.setRequired(true)

      builder.analyzeHeaders(h.rule) must equal(List(hp1, hp2))
    }
  }

  "ApiBuilder.collectPaths" should {

    "find a simple path" in {
      val p0 = "foo"
      val List((path, p1)) = builder.collectPaths(p0.rule :: Nil)

     (path should_== "/foo") && (p1 must equal(new m.Path))
    }

    "find a simple AND path" in {
      val p0 = "foo" / "bar"
      val List((path, p1)) = builder.collectPaths(p0.rule :: Nil)

      (path should_== "/foo/bar") && (p1 must equal (new m.Path))
    }

    "find a simple OR path" in {
      val p0 = "foo" || "bar"
      val List((path1, p1), (path2, p2)) = builder.collectPaths(p0.rule :: Nil)

      (path1 should_== "/foo") && (p1 must equal(new m.Path)) &&
      (path2 should_== "/bar") && (p2 must equal(new m.Path))
    }

    "find a capture OR path" in {
      val p0 = pathVar[Int]("foo") || pathVar[Int]("bar")
      val pp1 = (new PathParameter).name("foo")
      pp1.setType("integer")
      pp1.setRequired(true)
      val pp2 = (new PathParameter).name("bar")
      pp2.setType("integer")
      pp2.setRequired(true)
      val List((ps1, p1), (ps2, p2)) = builder.collectPaths(p0.rule :: Nil)

      (ps1 should_== "/{foo}") && (p1.getParameters.size should_== 1) && (p1.getParameters.get(0) must equal(pp1))
      (ps2 should_== "/{bar}") && (p2.getParameters.size should_== 1) && (p2.getParameters.get(0) must equal(pp2))
    }

    "find a simple path with a capture" in {
      val p0 = "foo" / pathVar[Int]("number")
      val pp1 = (new PathParameter).name("number")
      pp1.setType("integer")
      pp1.setRequired(true)

      val List((ps, p1)) = builder.collectPaths(p0.rule :: Nil)

      (ps should_== "/foo/{number}") &&
      (p1.getParameters.size should_== 1) &&
      (p1.getParameters.get(0) must equal(p1.getParameters.get(0)))
    }
  }

  "ApiBuilder.mkOperation" should {

    import DummyCompiler.compilerInstance

    "Get a route description" in {
      val r = "foo" ** GET / "bar" |>> { () => "" }
      val op = builder.mkOperation("/bar", r)

      op.getSummary must_== "foo"
    }

    "Get available models" in {
      import dummy._
      import org.http4s.rho.bits.ResponseGeneratorInstances._

      implicit def renderableEncoder[T <: Renderable]: EntityEncoder[T] =
        EntityEncoder
          .stringEncoder(Charset.`UTF-8`)
          .contramap { r: T => "" }
          .withContentType(`Content-Type`(MediaType.`application/json`, Charset.`UTF-8`))

      val r = "testing models" ** GET / "models" |>> { () =>
        val a = 0
        a match {
          case 0 => Ok(ModelA("modela", 1))
          case 1 => NotFound(ModelB("modelb", 2))
          case 2 => PreconditionFailed(ModelC("modelc", "round"))
        }
      }

      builder.actionToApiListing(r)

      import scala.collection.JavaConversions._

      swagger.getDefinitions.toList.map(_._1) must_== List("ModelA", "ModelB", "ModelC")
    }
  }
}
