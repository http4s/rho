package com.http4s.rho.hal.plus.swagger.demo

sealed trait MessageType
case object Error extends MessageType
case object Info extends MessageType
case object Warning extends MessageType
case class Message(text: String, `type`: MessageType)
