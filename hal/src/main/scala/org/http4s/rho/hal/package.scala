package org.http4s.rho

import org.http4s._
import org.http4s.CharacterSet._
import org.http4s.Header._

package object hal {

  /** Hypertext Application Language Type */
  sealed trait HalType

  /**
   * A Link Object represents a hyperlink from the containing resource to a URI.
   *
   * @param href The "href" property is REQUIRED.
   *
   * Its value is either a URI [RFC3986] or a URI Template [RFC6570].
   *
   * If the value is a URI Template then the Link Object SHOULD have a
   * "templated" attribute whose value is `true`.
   *
   * @param templated The "templated" property is OPTIONAL.
   *
   * Its value is boolean and SHOULD be true when the Link Object's "href"
   * property is a URI Template.
   *
   * Its value SHOULD be considered false if it is undefined or any other
   * value than true.
   *
   * @param type The "type" property is OPTIONAL.
   *
   * Its value is a string used as a hint to indicate the media type
   * expected when dereferencing the target resource.
   *
   * @param deprecation The "deprecation" property is OPTIONAL.
   *
   * Its presence indicates that the link is to be deprecated (i.e.
   * removed) at a future date.  Its value is a URL that SHOULD provide
   * further information about the deprecation.
   *
   * A client SHOULD provide some notification (for example, by logging a
   * warning message) whenever it traverses over a link that has this
   * property.  The notification SHOULD include the deprecation property's
   * value so that a client manitainer can easily find information about
   * the deprecation.
   *
   * @param name The "name" property is OPTIONAL.
   *
   * Its value MAY be used as a secondary key for selecting Link Objects
   * which share the same relation type.
   *
   * @param profile The "profile" property is OPTIONAL.
   *
   * Its value is a string which is a URI that hints about the profile (as
   * defined by [I-D.wilde-profile-link]) of the target resource.
   *
   * @param title The "title" property is OPTIONAL.
   *
   * Its value is a string and is intended for labelling the link with a
   * human-readable identifier (as defined by [RFC5988]).
   *
   * @param hreflang The "hreflang" property is OPTIONAL.
   *
   * Its value is a string and is intended for indicating the language of
   * the target resource (as defined by [RFC5988]).
   */
  case class LinkObject( // TODO think about usage of `org.http4s.Uri`
    href: String,
    templated: Option[Boolean] = None,
    `type`: Option[String] = None,
    deprecation: Option[String] = None,
    name: Option[String] = None,
    profile: Option[String] = None,
    title: Option[String] = None,
    hreflang: Option[String] = None)
    extends HalType {
    Predef.require(href != null, "href must not be null")
    Predef.require(href.nonEmpty, "href must not be empty")
  }

  /**
   * A Resource Object represents a resource.
   *
   * @param links contains links to other resources
   *
   * The "links" property is OPTIONAL.
   *
   * It is an object whose property names are link relation types (as
   * defined by [RFC5988]) and values are either a Link Object or an array
   * of Link Objects.  The subject resource of these links is the Resource
   * Object of which the containing "_links" object is a property.
   *
   * @param embedded contains embedded resources
   *
   * The "embedded" property is OPTIONAL
   *
   * It is an object whose property names are link relation types (as
   * defined by [RFC5988]) and values are either a Resource Object or an
   * array of Resource Objects.
   *
   * Embedded Resources MAY be a full, partial, or inconsistent version of
   * the representation served from the target URI.
   *
   * @param content represents an object that contains all other properties
   *
   * All properties of the content instance will be rendered as JSON, and
   * represent the current state of the resource.
   *
   */
  case class ResourceObject[T](
    links: Links = Vector.empty,
    embedded: Embedded = Vector.empty,
    content: Option[T] = None)
    extends HalType

  type EmbeddedDef = (String, Entry[ResourceObject[_]])

  type Embedded = Vector[EmbeddedDef]

  type LinkObjectDef = (String, Entry[LinkObject])

  type Links = Vector[LinkObjectDef]

  sealed abstract class Entry[+A]
  final case class Single[+A](x: A) extends Entry[A]
  final case class Many[+A](xs: Vector[A]) extends Entry[A]
  object Many {
    def empty[A]: Many[A] = Many()
    def apply[A](xs: A*) = new Many(xs.toVector)
  }

}
