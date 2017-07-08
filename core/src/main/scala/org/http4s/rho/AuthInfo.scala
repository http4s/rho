package org.http4s.rho

/** Typeclass for authentication embedding and retrieval from [[org.http4s.Request]] attributes map
  *
  * @tparam U Authentication info type for ex: User(id: UUID...)
  */
trait AuthInfo[U] {

  /* AttributeKey name for authInfo in request attributes map */
  final val attrKey = "authInfo"

  /* Serialize authInfo to a String */
  def serialize(u: U): String

  /* Deserialize authInfo from a String */
  def fromString(sv: String): U
}

