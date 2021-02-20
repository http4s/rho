package org.http4s.rho.hal

import scala.collection.mutable.LinkedHashMap
import scala.collection.immutable.Seq
import org.http4s.Uri
import org.http4s.UriTemplate

class ResourceObjectBuilder[T, E] {
  private val _links = LinkedHashMap[String, Either[LinkObject, Seq[LinkObject]]]()
  private val _embedded =
    LinkedHashMap[String, Either[ResourceObject[E, _], Seq[ResourceObject[E, _]]]]()
  private var content: Option[T] = None

  def content(c: T): this.type = {
    content = if (c == null) None else Some(c)
    this
  }

  /** Creates a single link object with a given `href` and the specified `name`
    * to this document builder. In case the same `name` already exists the link
    * object will be overwritten.
    */
  def link(name: String, href: String, templated: Option[Boolean] = None): this.type =
    link(name, LinkObject(href, templated = templated))

  /** Creates a single link object with a given `href` and the specified `name`
    * to this document builder. In case the same `name` already exists the link
    * object will be overwritten.
    */
  def link(name: String, href: String, title: String): this.type =
    link(name, LinkObject(href, title = Some(title)))

  /** Creates a single link object with a given `Uri` as `href` and the
    * specified `name` to this document builder. In case the same `name` already
    * exists the link object will be overwritten.
    */
  def link(name: String, href: Uri): this.type =
    link(name, LinkObject(href.toString))

  /** Creates a single link object with a given `UriTemplate` as `href` and
    * the specified `name` to this document builder. In case the same `name`
    * already exists the link object will be overwritten.
    */
  def link(name: String, href: UriTemplate): this.type =
    link(name, LinkObject(href.toString, templated = Some(true)))

  /** Puts a single link object with the specified `name` to this document
    * builder. In case the same `name` already exists the link object will be
    * overwritten.
    */
  def link(name: String, linkObj: LinkObject): this.type = {
    _links.put(name, Left(linkObj))
    this
  }

  /** Puts a list of link objects with the specified `name` to this document
    * builder. In case the same `name` already exists the link objects will be
    * overwritten.
    */
  def links(name: String, linkObjs: List[LinkObject]): this.type = {
    _links.put(name, Right(linkObjs))
    this
  }

  /** Puts an array of link objects with the specified `name` to this document
    * builder. In case the same `name` already exists the link objects will be
    * overwritten.
    */
  def links(name: String, linkObjs: LinkObject*): this.type =
    links(name, linkObjs.toList)

  /** Puts a single resource object with the specified `name` to this
    * document builder. In case the same `name` already exists the resource
    * object will be overwritten.
    */
  def resource(name: String, resObj: ResourceObject[E, _]): this.type = {
    _embedded.put(name, Left(resObj))
    this
  }

  /** Puts a list of resource objects with the specified `name` to this
    * document builder. In case the same `name` already exists the resource
    * objects will be overwritten.
    */
  def resources(name: String, resObjs: List[ResourceObject[E, _]]): this.type = {
    _embedded.put(name, Right(resObjs))
    this
  }

  /** Puts an array of resource objects with the specified `name` to this
    * document builder. In case the same `name` already exists the resource
    * objects will be overwritten.
    */
  def resources(name: String, resObjs: ResourceObject[E, _]*): this.type =
    resources(name, resObjs.toList)

  def build(): ResourceObject[T, E] = ResourceObject(_links.toList, _embedded.toList, content)

}
