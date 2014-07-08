package org.http4s.rho
package hal

import org.json4s._
import org.json4s.Extraction._

object ResourceObjectSerializer {

  def serialize(r: ResourceObject[_])(implicit jsonFormats: Formats): JValue = {
    val links = serializeLinks(r.links)
    val embedded = serializeEmbedded(r.embedded)
    val root: JValue = r.content match {
      case Some(v) =>
        val content = decompose(v)
        content match {
          case JObject(fields) =>
            if (links.isDefined && embedded.isDefined)
              JObject(links.get :: embedded.get :: fields)
            else if (links.isDefined)
              JObject(links.get :: fields)
            else if (embedded.isDefined)
              JObject(embedded.get :: fields)
            else JObject(fields)
          case v: JValue => v
        }
      case _ =>
        if (links.isDefined && embedded.isDefined)
          JObject(links.get, embedded.get)
        else if (links.isDefined)
          JObject(links.get)
        else if (embedded.isDefined)
          JObject(embedded.get)
        else JObject()
    }
    root
  }

  private[hal] def serializeEmbedded(embedded: Embedded)(implicit jsonFormats: Formats): Option[JField] = {
    val embeddedAsFields = for {
      fieldOption <- embedded map serializeEmbeddedDef
      field <- fieldOption
    } yield field
    if (embeddedAsFields.isEmpty)
      None
    else
      Some(JField("_embedded", JObject(embeddedAsFields.toList)))
  }

  private[hal] def serializeEmbeddedDef(embeddedDef: (String, Entry[ResourceObject[_]]))(implicit jsonFormats: Formats): Option[JField] =
    serializeSingleOrMany(embeddedDef)(ResourceObjectSerializer.serialize)

  private[hal] def serializeLinkDef(linkDef: (String, Entry[LinkObject])): Option[JField] =
    serializeSingleOrMany(linkDef)(LinkObjectSerializer.serialize)

  private[hal] def serializeLinks(links: Links): Option[JField] = {
    val linksAsFields = for {
      fieldOption <- links map serializeLinkDef
      field <- fieldOption
    } yield field
    if (linksAsFields.isEmpty)
      None
    else
      Some(JField("_links", JObject(linksAsFields.toList)))
  }

  private[hal] def serializeSingleOrMany[T](linkDef: (String, Entry[T]))(f: T => JValue): Option[JField] = linkDef._2 match {
    case Many(vs) =>
      val xs = vs map f
      Some(JField(linkDef._1, JArray(xs.toList)))
    case Single(v) =>
      Some(JField(linkDef._1, f(v)))
    case _ =>
      None
  }

}

class ResourceObjectSerializer extends CustomSerializer[ResourceObject[_]](format => (
  PartialFunction.empty,
  {
    case r: ResourceObject[_] =>
      ResourceObjectSerializer.serialize(r)(format)
  }))
