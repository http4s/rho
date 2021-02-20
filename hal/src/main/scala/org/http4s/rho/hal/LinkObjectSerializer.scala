package org.http4s.rho.hal

import org.json4s._

import scala.collection.mutable.ArrayBuffer

object LinkObjectSerializer {
  val serialize: PartialFunction[Any, JValue] = { case l: LinkObject =>
    serialize(l)
  }
  def serialize(l: LinkObject): JObject = {
    val b = new ArrayBuffer[JField]()
    b.append(JField("href", JString(l.href)))
    if (l.templated.isDefined)
      b.append(JField("templated", JBool(l.templated.get)))
    if (l.`type`.isDefined)
      b.append(JField("type", JString(l.`type`.get)))
    if (l.deprecation.isDefined)
      b.append(JField("deprecation", JString(l.deprecation.get)))
    if (l.name.isDefined)
      b.append(JField("name", JString(l.name.get)))
    if (l.profile.isDefined)
      b.append(JField("profile", JString(l.profile.get)))
    if (l.title.isDefined)
      b.append(JField("title", JString(l.title.get)))
    if (l.hreflang.isDefined)
      b.append(JField("hreflang", JString(l.hreflang.get)))
    JObject(b.toList)
  }
}

class LinkObjectSerializer
    extends CustomSerializer[LinkObject](_ =>
      (PartialFunction.empty, LinkObjectSerializer.serialize)
    )
