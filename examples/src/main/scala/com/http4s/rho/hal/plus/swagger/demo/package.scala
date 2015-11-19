package com.http4s.rho.hal.plus.swagger

import scala.language.implicitConversions
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.http4s.Charset
import org.http4s.headers.`Content-Type`
import org.http4s.MediaType
import org.http4s.EntityEncoder
import org.http4s.rho.bits.RequestRuleAST.TypedRequestRule
import org.http4s.rho.hal.LinkObjectSerializer
import org.http4s.rho.hal.ResourceObject
import org.http4s.rho.hal.ResourceObjectSerializer
import org.json4s.DefaultFormats
import org.json4s.Extraction.decompose
import org.json4s.Formats
import org.json4s.JsonAST.JValue
import org.json4s.ext.JodaTimeSerializers
import org.json4s.jackson.JsonMethods.compact
import org.json4s.jackson.JsonMethods.render

package object demo {

  ///// implicit helper functions ///// 

  /** Defines the defaults to render known and unknown types to JSON */
  implicit val jsonFormats: Formats =
    DefaultFormats ++
      JodaTimeSerializers.all +
      new LinkObjectSerializer +
      new ResourceObjectSerializer

  implicit def resourceObjectAsJsonEncoder[A, B]: EntityEncoder[ResourceObject[A, B]] =
    EntityEncoder
      .stringEncoder(Charset.`UTF-8`)
      .contramap { r: ResourceObject[A, B] => compact(render(json(r))) }
      .withContentType(`Content-Type`(MediaType.`application/hal+json`, Charset.`UTF-8`))

  implicit def messageAsJsonEncoder: EntityEncoder[Message] =
    EntityEncoder
      .stringEncoder(Charset.`UTF-8`)
      .contramap { r: Message => compact(render(json(r))) }
      .withContentType(`Content-Type`(MediaType.`application/json`, Charset.`UTF-8`))

  /** Extracts the name of the first query parameter as string */
  implicit def paramName(q: TypedRequestRule[_]): String = q.queryNames.head

  ///// regular helper functions ///// 

  /**
   * Converts a sequence of `Try` into a `Try` of sequences. In case failures
   * are in there the first failure will be returned.
   */
  def flattenTry[T](xs: Seq[Try[T]]): Try[Seq[T]] = {
    val (ss: Seq[Success[T]] @unchecked, fs: Seq[Failure[T]] @unchecked) = xs.partition(_.isSuccess)
    if (fs.isEmpty) Success(ss map (_.get))
    else Failure[Seq[T]](fs(0).exception) // Only keep the first failure
  }

  /** Converts a given object into a JSON structure */
  def json[A <: Equals](a: A): JValue =
    decompose(a)

}
