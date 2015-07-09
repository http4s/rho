package org.http4s.rho.bits

import org.http4s.Status
import scala.reflect.runtime.universe.{ Type, typeOf }


sealed trait ResultInfo
case class TypeOnly(tpe: Type) extends ResultInfo
case class StatusAndType(status: Status, tpe: Type) extends ResultInfo
case class StatusOnly(status: Status) extends ResultInfo
