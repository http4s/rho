package org.http4s.rho.hal

/** A Resource Object represents a resource.
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
  */
case class ResourceObject[T, E](
    links: Links = Nil,
    embedded: Embedded[E] = Nil,
    content: Option[T] = None)
