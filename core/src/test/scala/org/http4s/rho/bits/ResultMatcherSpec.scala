package org.http4s
package rho
package bits

import org.http4s.rho
import org.specs2.mutable.Specification

import scala.reflect.runtime.universe._
import shapeless.HList
import Status._
import fs2.Chunk

class ResultMatcherSpec extends Specification {

  class TRhoService
    extends bits.MethodAliases
    with bits.ResponseGeneratorInstances
  {
    var statuses: Set[(Status, Type)] = Set.empty

    implicit final protected val compileSrvc: CompileService[RhoRoute.Tpe] = {
      new CompileService[RhoRoute.Tpe] {
        override def compile[T <: HList](route: RhoRoute[T]): RhoRoute.Tpe = {
          statuses = route.resultInfo.collect { case StatusAndType(s, t) => (s, t) }
          route
        }
      }
    }
  }

  "ResponseGenerator" should {

    "Match a single result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () => Ok("updated").unsafeRun }
      }

      srvc.statuses.map(_._1) should_== Set(Ok)
    }

    "Match two results with different status with different result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () =>
          val a = 0
          a match {
            case 0 => NotFound(s"Not found")
            case 1 => Ok("Hello world".getBytes())
          }
        }
      }

      srvc.statuses.map(_._1) should_== Set(NotFound, Ok)
      srvc.statuses.collect{ case (NotFound, t) => t }.head =:= weakTypeOf[String] must_== true
      srvc.statuses.collect{ case (Ok, t) => t }.head =:= weakTypeOf[Array[Byte]] must_== true
    }

    "Match two results with same stat different result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () =>
          val a = 0
          a match {
            case 0 => Ok(s"Not found")
            case 1 => Ok("Hello world".getBytes())
          }
        }
      }

      srvc.statuses.map(_._1) should_== Set(Ok)
    }

    "Match an empty result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () => NoContent() }
      }

      srvc.statuses.map(_._1) should_== Set(NoContent)
      srvc.statuses.head._2 =:= weakTypeOf[org.http4s.rho.bits.ResponseGenerator.EmptyRe]
    }

    "Match three results with different status but same result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () =>
          val a = 0
          a match {
            case 0 => NotFound(s"Not found")
            case 1 => Ok("updated")
            case 2 => Accepted("it was accepted")
          }
        }
      }

      srvc.statuses.map(_._1) should_== Set(NotFound, Ok, Accepted)
    }

    "Match four results with different status but same result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () =>
          val a = 0
          a match {
            case 0 => NotFound(s"Not found")
            case 1 => Ok("updated")
            case 2 => Accepted("it was accepted")
            case 4 => Created("it was created")
          }
        }
      }

      srvc.statuses.map(_._1) should_== Set(NotFound, Ok, Accepted, Created)
    }

    "Match results with locally defined types" in {
      import scodec.bits.ByteVector

      case class ModelA(name: String, color: Int)
      case class ModelB(name: String, id: Long)

      implicit def w1: EntityEncoder[ModelA] = EntityEncoder.simple[ModelA]()(_ => Chunk.bytes("A".getBytes))
      implicit def w2: EntityEncoder[ModelB] = EntityEncoder.simple[ModelB]()(_ => Chunk.bytes("B".getBytes))

      val srvc = new TRhoService {
        GET / "foo" |>> { () =>
          if (true) Ok(ModelA("test ok", 1))
          else NotFound(ModelB("test not found", 234))
        }
      }

      srvc.statuses.map(_._1) should_== Set(Ok, NotFound)

      // the type equality for locally defined types is a bit "murkey" so we use the String name
      srvc.statuses.collect{ case (Ok, t) => t }.head.toString must_== "ModelA"
      srvc.statuses.collect{ case (NotFound, t) => t }.head.toString must_== "ModelB"
    }

    "Match complex models as well as simple ones" in {
      import Foo._

      val srvc = new TRhoService {
        GET / "foo" |>> { () =>
          if (true) Ok(FooA("test ok", 1))
          else NotFound(FooB("test not found", 234))
        }
      }

      srvc.statuses.map(_._1) should_== Set(Ok, NotFound)
      srvc.statuses.collect{ case (Ok, t) => t }.head =:= weakTypeOf[FooA] must_== true
      srvc.statuses.collect{ case (NotFound, t) => t }.head =:= weakTypeOf[FooB] must_== true

    }
  }
}

object Foo {
  case class FooA(name: String, color: Int)
  case class FooB(name: String, id: Long)

  implicit def w1: EntityEncoder[FooA] = EntityEncoder.simple[FooA]()(_ => Chunk.bytes("A".getBytes))
  implicit def w2: EntityEncoder[FooB] = EntityEncoder.simple[FooB]()(_ => Chunk.bytes("B".getBytes))
}
