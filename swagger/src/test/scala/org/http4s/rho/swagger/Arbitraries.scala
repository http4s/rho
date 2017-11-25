package org.http4s.rho.swagger

import models._
import org.scalacheck._
import Gen._
import Arbitrary._
import org.http4s.MediaType
import MediaType._
import cats.{Applicative, Functor, Monad}
import cats.syntax.all._
import cats.instances.all._

import scala.annotation.tailrec

/**
 * Arbitraries for the creation of Swagger models
 *
 * They can be improved by
 *  - generating more data where Gen.const or Map.empty is used
 *  - making sure the models are coherent in terms of parameters / properties / responses / definitions
 */
object Arbitraries {

  implicit def ArbitrarySwagger: Arbitrary[Swagger] =
    Arbitrary {
      for {
        swagger              <- identifier
        info                 <- option(genInfo)
        host                 <- option(genHost)
        basePath             <- option(genBasePath)
        schemes              <- listOf(genScheme)
        consumes             <- listOf(genMediaType)
        produces             <- listOf(genMediaType)
        definitions          <- mapOf[Model]
        paths                <- mapOf(genPath(definitions))
        securityDefinitions  <- mapOf(genSecurityDefinition)
        parameters           <- mapOf[Parameter]
        externalDocs         <- option(genExternalDocs)
      } yield Swagger(
        swagger,
        info,
        host,
        basePath,
        schemes,
        consumes,
        produces,
        paths,
        securityDefinitions,
        definitions,
        parameters,
        externalDocs
      )
    }

  def genHost: Gen[String] =
    const("rho.org")

  def genBasePath: Gen[String] =
    const("/")

  def genInfo: Gen[Info] =
    const(Info("title", "version"))

  def genScheme: Gen[Scheme] =
    oneOf(Scheme.HTTP, Scheme.HTTPS, Scheme.WS, Scheme.WSS)

  def genMediaType: Gen[String] =
    oneOf(
      `application/json`,
      `audio/midi`,
      `image/gif`,
      `text/html`
    ).map(_.renderString)

  def listOf[T : Arbitrary]: Gen[List[T]] =
    listOf(arbitrary[T])

  def listOf[T](gen: Gen[T]): Gen[List[T]] =
    choose(0, 4).flatMap(n => listOfN(n, gen))

  def mapOf[T : Arbitrary]: Gen[Map[String, T]] =
    mapOf(arbitrary[T])

  def mapOf[T](gen: Gen[T]): Gen[Map[String, T]] =
    choose(1, 10).flatMap(n => listOfN(n, (identifier |@| gen).tupled).map(_.toMap))

  def genPath(definitions: Map[String, Model]): Gen[Path] =
    for {
      get              <- option(genOperation(definitions))
      put              <- option(genOperation(definitions))
      post             <- option(genOperation(definitions))
      delete           <- option(genOperation(definitions))
      patch            <- option(genOperation(definitions))
      options          <- option(genOperation(definitions))
      head             <- option(genOperation(definitions))
      parameters       <- listOf[Parameter]
      vendorExtensions <- const(Map.empty[String, Any])
    } yield Path(
        get,
        put,
        post,
        delete,
        patch,
        options,
        head,
        parameters,
        vendorExtensions
      )

  def genOperation(definitions: Map[String, Model]): Gen[Operation] = for {
    tags             <- listOf(identifier)
    summary          <- option(identifier)
    description      <- option(identifier)
    operationId      <- option(identifier)
    schemes          <- listOf(genScheme)
    consumes         <- listOf(genMediaType)
    produces         <- listOf(genMediaType)
    parameters       <- listOf[Parameter]
    responses        <- mapOf(genResponse(definitions))
    security         <- const(Nil)
    externalDocs     <- option(genExternalDocs)
    deprecated       <- arbitrary[Boolean]
    vendorExtensions <- const(Map.empty[String, Any])
  } yield
    Operation(
      tags,
      summary,
      description,
      operationId,
      schemes,
      consumes,
      produces,
      parameters,
      responses,
      security,
      externalDocs,
      deprecated,
      vendorExtensions
    )

  def genResponse(definitions: Map[String, Model]): Gen[Response] = for {
    description <- identifier
    schema      <- option(oneOf(definitions.keys.toList).map(p => RefProperty(p)))
    examples    <- mapOf(genExample)
    headers     <- mapOf[Property]
  } yield
    Response(
      description,
      schema,
      examples,
      headers
    )

  def genExample: Gen[String] =
    const("an example")


  implicit def ArbitraryModel: Arbitrary[Model] =
    Arbitrary {
      for {
        id                   <- identifier
        id2                  <- identifier
        description          <- option(identifier)
        `type`               <- option(identifier)
        name                 <- option(identifier)
        properties           <- mapOf(arbitrary[Property])
        required             <- choose(0, properties.size - 1).flatMap(n => const(properties.take(n).keys.toList))
        isSimple             <- arbitrary[Boolean]
        example              <- option(identifier)
        additionalProperties <- Gen.const(None)
        discriminator        <- Gen.const(None)
        externalDocs         <- Gen.const(None)
      } yield
        ModelImpl(
          id,
          id2,
          description,
          `type`,
          name,
          required,
          properties,
          isSimple,
          example,
          additionalProperties,
          discriminator,
          externalDocs
        )
    }


  def genSecurityDefinition: Gen[SecuritySchemeDefinition] =
    Gen.oneOf(
      OAuth2Definition("authorizationUrl", "tokenUrl", "flow", Map.empty),
      OAuth2VendorExtensionsDefinition("authorizationUrl", Map("x-issuer"->"issuer", "x-audiences"->"audience"), "flow", Map.empty),
      ApiKeyAuthDefinition("name", In.HEADER),
      BasicAuthDefinition)

  implicit def ArbitraryParameter: Arbitrary[Parameter] =
    Arbitrary {
      Gen.oneOf(
        BodyParameter(),
        CookieParameter("string"),
        FormParameter("string"),
        HeaderParameter("string"),
        PathParameter("string"),
        QueryParameter(),
        RefParameter("ref")
      )
    }

  implicit def ArbitraryProperty: Arbitrary[Property] =
    Arbitrary {
      oneOf(
        const(AbstractProperty(): Property),
        oneOf(AbstractProperty(), RefProperty("ref")).map(p => ArrayProperty(p)),
        const(RefProperty("ref"))
      )
    }

  def genExternalDocs: Gen[ExternalDocs] =
    Gen.const(ExternalDocs("description", "url"))

  implicit def GenApplicative: Applicative[Gen] = new Applicative[Gen] {
    def pure[A](x: A): Gen[A] = Gen.const(x)
    def ap[A, B](ff: Gen[(A) => B])(fa: Gen[A]): Gen[B] = {
      fa.flatMap { a =>
        ff.map(f => f(a))
      }
    }
  }

}
