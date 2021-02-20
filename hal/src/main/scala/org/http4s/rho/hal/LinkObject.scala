package org.http4s.rho.hal

import org.http4s.Uri
import org.http4s.UriTemplate

/** A Link Object represents a hyperlink from the containing resource to a URI.
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
trait LinkObjectLike {
  def href: String
  def templated: Option[Boolean] = None
  def `type`: Option[String] = None
  def deprecation: Option[String] = None
  def name: Option[String] = None
  def profile: Option[String] = None
  def title: Option[String] = None
  def hreflang: Option[String] = None
}

/** Represents the default implementation of a Link Object which all aspects of
  * the specification.
  */
case class LinkObject(
    href: String,
    override val name: Option[String] = None,
    override val title: Option[String] = None,
    override val templated: Option[Boolean] = None,
    override val `type`: Option[String] = None,
    override val deprecation: Option[String] = None,
    override val profile: Option[String] = None,
    override val hreflang: Option[String] = None)
    extends LinkObjectLike {
  require(href != null, "href must not be null")
  require(href.nonEmpty, "href must not be empty")
}

object LinkObject {

  def fromUri(
      href: Uri,
      name: Option[String] = None,
      title: Option[String] = None,
      `type`: Option[String] = None,
      deprecation: Option[String] = None,
      profile: Option[String] = None,
      hreflang: Option[String] = None): LinkObject =
    new LinkObject(href.toString, name, title, None, `type`, deprecation, profile, hreflang)

  def fromTemplate(
      href: UriTemplate,
      name: Option[String] = None,
      title: Option[String] = None,
      `type`: Option[String] = None,
      deprecation: Option[String] = None,
      profile: Option[String] = None,
      hreflang: Option[String] = None): LinkObject =
    new LinkObject(href.toString, name, title, Some(true), `type`, deprecation, profile, hreflang)

}
