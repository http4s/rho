package org.http4s.rho.bits

import org.http4s.Status
import scala.reflect.runtime.universe.{ Type, TypeTag, typeOf }


sealed trait ResultInfo
case class ModelOnly(tpe: Type) extends ResultInfo
case class StatusAndModel(status: Status, tpe: Type) extends ResultInfo
case class StatusOnly(status: Status) extends ResultInfo
case object Empty extends ResultInfo

object ResultInfo {

  // TODO: Finish this map!
  private lazy val statusIndex: Set[(Type, Status)] = Set(
    (typeOf[Status.Ok.type], Status.Ok),
    (typeOf[Status.NotFound.type], Status.NotFound)
  )
  
  def getStatus(tpe: Type): Option[Status] = statusIndex.find{ case (t2, s) => tpe =:= t2 }.map(_._2)
}