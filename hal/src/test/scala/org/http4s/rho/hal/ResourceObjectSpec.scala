package org.http4s.rho.hal

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import org.specs2.mutable.Specification

object ResourceObjectSpec extends Specification {
  implicit val jsonFormats: Formats = DefaultFormats

  case class User(name: String, email: String)
  val user1 = User("Max", "max@example.com")

  "ResourceObject serialization" should {
    "serialize also if empty" in {
      val resObj = ResourceObject()
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        JObject()
    }
    "with one link only" in {
      val resObj = ResourceObject(
        Vector("self" ->
          Single(LinkObject("/some/path"))))
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        ("_links" ->
          ("self" ->
            ("href", "/some/path")))
    }
    "with two links only" in {
      val resObj = ResourceObject(
        Vector("self" ->
          Many(
            LinkObject("/some/path/1"),
            LinkObject("/some/path/2"))))
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        ("_links" ->
          ("self" ->
            List(
              ("href", "/some/path/1"),
              ("href", "/some/path/2"))))
    }
    "with one embedded only" in {
      val resObj = ResourceObject(
        Vector.empty,
        Vector("text" -> Single(ResourceObject(content = Some("some content")))))
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        ("_embedded" ->
          ("text" ->
            ("some content")))
    }
    "with two embedded only" in {
      val resObj = ResourceObject(
        Vector.empty,
        Vector("texts" ->
          Many(
            ResourceObject(content = Some("/some/path/1")),
            ResourceObject(content = Some("/some/path/2")))))
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        ("_embedded" ->
          ("texts" ->
            List(
              ("/some/path/1"),
              ("/some/path/2"))))
    }
    "with non-primitive content without links and embedded definitions" in {
      val resObj = ResourceObject(content = Some(user1))
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        (("name" -> "Max") ~ ("email" -> "max@example.com"))
    }
    "with non-primitive content, links and embedded definitions will be ignored" in {
      val resObj = ResourceObject(
        content = Some(user1),
        links =
          Vector("self" ->
            Single(
              LinkObject("/users/1"))),
        embedded =
          Vector("groups" ->
            Many(
              ResourceObject(content = Some("/groups/1")),
              ResourceObject(content = Some("/groups/2")))))
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        (
          ("_links" -> ("self" -> ("href", "/users/1"))) ~
          ("_embedded" -> ("groups" -> List("/groups/1", "/groups/2"))) ~
          ("name" -> "Max") ~
          ("email" -> "max@example.com"))
    }
    "with primitive content, links and embedded definitions will be ignored" in {
      val resObj = ResourceObject(
        content = Some("some content"),
        links =
          Vector("self" ->
            Single(LinkObject("/some/path"))),
        embedded =
          Vector("text" ->
            Single(ResourceObject(content = Some("some other content")))))
      ResourceObjectSerializer.serialize(resObj) must be equalTo
        ("some content")
    }
  }

  "An example from HAL specification" should {

    // an example from http://stateless.co/hal_specification.html
    val json = parse("""
{
    "_links": {
        "self": { "href": "/orders" },
        "curies": [{ "href": "http://example.com/docs/rels/{rel}", "templated": true, "name": "ea" }],
        "next": { "href": "/orders?page=2" },
        "ea:find": {
            "href": "/orders{?id}",
            "templated": true
        },
        "ea:admin": [{
            "href": "/admins/2",
            "title": "Fred"
        }, {
            "href": "/admins/5",
            "title": "Kate"
        }]
    },
    "_embedded": {
        "ea:order": [{
            "_links": {
                "self": { "href": "/orders/123" },
                "ea:basket": { "href": "/baskets/98712" },
                "ea:customer": { "href": "/customers/7809" }
            },
            "total": 30.00,
            "currency": "USD",
            "status": "shipped"
        }, {
            "_links": {
                "self": { "href": "/orders/124" },
                "ea:basket": { "href": "/baskets/97213" },
                "ea:customer": { "href": "/customers/12369" }
            },
            "total": 20.00,
            "currency": "USD",
            "status": "processing"
        }]
    },
    "currentlyProcessing": 14,
    "shippedToday": 20
}""")

    // our data structure
    val halDocument = ResourceObject(

      links = Vector(
        "self" -> Single(LinkObject("/orders")),
        "curies" -> Many(LinkObject(name = Some("ea"), href = "http://example.com/docs/rels/{rel}", templated = Some(true))),
        "next" -> Single(LinkObject("/orders?page=2")),
        "ea:find" -> Single(LinkObject("/orders{?id}", templated = Some(true))),
        "ea:admin" -> Many(LinkObject("/admins/2", title = Some("Fred")), LinkObject("/admins/5", title = Some("Kate")))),

      embedded = Vector(
        "ea:order" ->
          Many(
            ResourceObject[Map[String, Any]](
              Vector(
                "self" -> Single(LinkObject("/orders/123")),
                "ea:basket" -> Single(LinkObject("/baskets/98712")),
                "ea:customer" -> Single(LinkObject("/customers/7809"))),
              Vector.empty,
              Some(Map("total" -> 30.00,
                "currency" -> "USD",
                "status" -> "shipped"))),
            ResourceObject[Map[String, Any]](
              Vector(
                "self" -> Single(LinkObject("/orders/124")),
                "ea:basket" -> Single(LinkObject("/baskets/97213")),
                "ea:customer" -> Single(LinkObject("/customers/12369"))),
              Vector.empty,
              Some(Map("total" -> 20.00,
                "currency" -> "USD",
                "status" -> "processing"))))),

      content = Some(
        Map(
          "currentlyProcessing" -> 14,
          "shippedToday" -> 20)))

    "be equal as pretty-printed JSON string when comparing to our data structure" in {
      writePretty(ResourceObjectSerializer.serialize(halDocument)) must be equalTo
        writePretty(json)
    }

    "be equal as JSON tree when comparing to our serialized data structure" in {
      ResourceObjectSerializer.serialize(halDocument) must be equalTo
        json
    }
  }

}
