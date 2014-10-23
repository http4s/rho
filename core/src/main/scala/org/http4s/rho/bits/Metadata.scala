package org.http4s
package rho.bits

trait Metadata

trait TextMetaData extends Metadata {
  def msg: String
}

case class RouteDescription(msg: String) extends TextMetaData
