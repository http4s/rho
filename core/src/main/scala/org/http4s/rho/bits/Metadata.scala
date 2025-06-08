/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s
package rho.bits

import scala.reflect.runtime.universe.TypeTag
import org.typelevel.ci.CIString

/** Base type for data that can be used to decorate the rules trees
  *
  * Metadata is not directly useful for evaluating a request or for generating HTTP responses; it is
  * primarily used for storing data to auto generate information about a route or service.
  */
trait Metadata

/** Textual meta data */
trait TextMetaData extends Metadata {
  def msg: String
}

/** Security Scope meta data */
trait SecurityScopesMetaData extends Metadata {
  def definitions: Map[String, List[String]] // Like `auth0_jwk` -> List('admin', user)
}

/** Metadata about a query rule */
case class QueryMetaData[F[_], T](
    name: String,
    description: Option[String],
    p: QueryParser[F, T],
    default: Option[T],
    m: TypeTag[T])
    extends Metadata

/** Metadata about a header rule */
case class HeaderMetaData[T](key: CIString, isRequired: Boolean) extends Metadata
