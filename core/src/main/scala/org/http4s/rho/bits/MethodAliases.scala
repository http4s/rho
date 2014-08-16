package org.http4s.rho.bits

import org.http4s.Method

object MethodAliases extends MethodAliases

trait MethodAliases {
  def OPTIONS = Method.OPTIONS
  def GET = Method.GET
  def HEAD = Method.HEAD
  def POST = Method.POST
  def PUT = Method.PUT
  def DELETE = Method.DELETE
  def TRACE = Method.TRACE
  def CONNECT = Method.CONNECT
  def PATCH = Method.PATCH
}
