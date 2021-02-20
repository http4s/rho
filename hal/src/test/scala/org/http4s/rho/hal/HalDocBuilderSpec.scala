package org.http4s.rho.hal

import org.http4s.Uri
import org.http4s.UriTemplate
import org.http4s.UriTemplate._
import org.specs2.mutable.Specification
import scala.collection.immutable.Seq

object ResourceObjectBuilderSpec extends Specification {

  "ResourceObjectBuilder" should {

    "create empty ResourceObject" in {
      new ResourceObjectBuilder().build() must equalTo(ResourceObject())
    }

    "create LinkObject from href" in {
      new ResourceObjectBuilder()
        .link("self", "/some/where")
        .build() must equalTo(
        ResourceObject(List("self" -> Left(LinkObject("/some/where", templated = None))))
      )
    }

    "create LinkObject from href and templated" in {
      new ResourceObjectBuilder()
        .link("self", "/some/where", Some(true))
        .build() must equalTo(
        ResourceObject(List("self" -> Left(LinkObject("/some/where", templated = Some(true)))))
      )
    }

    "create LinkObject from href and title" in {
      new ResourceObjectBuilder()
        .link("self", "/some/where", "A title")
        .build() must equalTo(
        ResourceObject(List("self" -> Left(LinkObject("/some/where", title = Some("A title")))))
      )
    }

    "create LinkObject from Uri" in {
      new ResourceObjectBuilder()
        .link("self", Uri(path = "/some/where"))
        .build() must equalTo(ResourceObject(List("self" -> Left(LinkObject("/some/where")))))
    }

    "create LinkObject from UriTemplate" in {
      new ResourceObjectBuilder()
        .link("self", UriTemplate(path = List(PathElm("some"), PathExp("where"))))
        .build() must equalTo(
        ResourceObject(List("self" -> Left(LinkObject("/some{/where}", templated = Some(true)))))
      )
    }

    val document = ResourceObject(
      links = List(
        "self" -> Left(LinkObject("/orders")),
        "curies" -> Right(
          Seq(
            LinkObject(
              name = Some("ea"),
              href = "http://example.com/docs/rels/{rel}",
              templated = Some(true)
            )
          )
        ),
        "next" -> Left(LinkObject("/orders?page=2")),
        "ea:find" -> Left(LinkObject("/orders{?id}", templated = Some(true))),
        "ea:admin" -> Right(
          Seq(
            LinkObject("/admins/2", title = Some("Fred")),
            LinkObject("/admins/5", title = Some("Kate"))
          )
        )
      ),
      embedded = List(
        "ea:order" ->
          Right(
            Seq(
              ResourceObject[Map[String, Any], Nothing](
                List(
                  "self" -> Left(LinkObject("/orders/123")),
                  "ea:basket" -> Left(LinkObject("/baskets/98712")),
                  "ea:customer" -> Left(LinkObject("/customers/7809"))
                ),
                Nil,
                Some(Map("total" -> 30.00, "currency" -> "USD", "status" -> "shipped"))
              ),
              ResourceObject[Map[String, Any], Nothing](
                List(
                  "self" -> Left(LinkObject("/orders/124")),
                  "ea:basket" -> Left(LinkObject("/baskets/97213")),
                  "ea:customer" -> Left(LinkObject("/customers/12369"))
                ),
                Nil,
                Some(Map("total" -> 20.00, "currency" -> "USD", "status" -> "processing"))
              )
            )
          )
      ),
      content = Some(Map("currentlyProcessing" -> 14, "shippedToday" -> 20))
    )

    // our data structure
    val documentBuilder =
      new ResourceObjectBuilder[Map[String, Int], Map[String, Any]]()
        .link("self", "/orders")
        .links(
          "curies",
          LinkObject(
            name = Some("ea"),
            href = "http://example.com/docs/rels/{rel}",
            templated = Some(true)
          )
        )
        .link("next", "/orders?page=2")
        .link("ea:find", "/orders{?id}", Some(true))
        .links(
          "ea:admin",
          LinkObject("/admins/2", title = Some("Fred")),
          LinkObject("/admins/5", title = Some("Kate"))
        )
        .resources(
          "ea:order",
          new ResourceObjectBuilder[Map[String, Any], Nothing]()
            .link("self", LinkObject("/orders/123"))
            .link("ea:basket", LinkObject("/baskets/98712"))
            .link("ea:customer", LinkObject("/customers/7809"))
            .content(Map("total" -> 30.00, "currency" -> "USD", "status" -> "shipped"))
            .build(),
          new ResourceObjectBuilder[Map[String, Any], Nothing]()
            .link("self", LinkObject("/orders/124"))
            .link("ea:basket", LinkObject("/baskets/97213"))
            .link("ea:customer", LinkObject("/customers/12369"))
            .content(Map("total" -> 20.00, "currency" -> "USD", "status" -> "processing"))
            .build()
        )
        .content(Map("currentlyProcessing" -> 14, "shippedToday" -> 20))
        .build()

    "build a ResourceObject" in {
      documentBuilder must be equalTo document
    }
  }

}
