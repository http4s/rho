package org.http4s
package rho
package bits

import cats.effect.IO
import fs2.Chunk
import munit.FunSuite
import org.http4s.rho.io._
import org.http4s.{Status => HStatus}
import shapeless.HList

import scala.reflect.runtime.universe._

class ResultMatcherSuite extends FunSuite {
  class TRhoRoutes[F[_]] extends bits.MethodAliases {
    var statuses: Set[(Status, Type)] = Set.empty

    implicit final protected def compileSrvc: CompileRoutes[F, RhoRoute.Tpe[F]] =
      new CompileRoutes[F, RhoRoute.Tpe[F]] {
        override def compile[T <: HList](route: RhoRoute[F, T]): RhoRoute.Tpe[F] = {
          statuses = route.resultInfo.collect { case StatusAndType(s, t) => (s, t) }
          route
        }
      }
  }

  test("A ResponseGenerator should match a single result type") {
    for {
      srvc <- Ok("updated").map(r =>
        new TRhoRoutes[IO] {
          PUT / "foo" |>> { () => r }
        }
      )
      _ = assertEquals(srvc.statuses.map(_._1), Set(Ok.status))
    } yield ()
  }

  test(
    "A ResponseGenerator should match two results with different status with different result type"
  ) {
    val srvc = new TRhoRoutes[IO] {
      PUT / "foo" |>> { () =>
        val a = 0
        a match {
          case 0 => NotFound(s"Not found")
          case 1 => Ok("Hello world".getBytes())
        }
      }
    }

    assertEquals(srvc.statuses.map(_._1), Set(NotFound.status, Ok.status))
    assert(
      srvc.statuses.collect { case (HStatus.NotFound, t) => t }.head =:= weakTypeOf[
        String
      ]
    )
    assert(
      srvc.statuses.collect { case (HStatus.Ok, t) => t }.head =:= weakTypeOf[
        Array[Byte]
      ]
    )
  }

  test("A ResponseGenerator should match two results with same stat different result type") {
    val srvc = new TRhoRoutes[IO] {
      PUT / "foo" |>> { () =>
        val a = 0
        a match {
          case 0 => Ok(s"Not found")
          case 1 => Ok("Hello world".getBytes())
        }
      }
    }

    assertEquals(srvc.statuses.map(_._1), Set(Ok.status))
  }

  test("A ResponseGenerator should match an empty result type") {
    val srvc = new TRhoRoutes[IO] {
      PUT / "foo" |>> { () => NoContent.apply }
    }

    assertEquals(srvc.statuses.map(_._1), Set(NoContent.status))
    assert(srvc.statuses.head._2 =:= typeOf[Unit])
  }

  test(
    "A ResponseGenerator should match three results with different status but same result type"
  ) {
    val srvc = new TRhoRoutes[IO] {
      PUT / "foo" |>> { () =>
        val a = 0
        a match {
          case 0 => NotFound(s"Not found")
          case 1 => Ok("updated")
          case 2 => Accepted("it was accepted")
        }
      }
    }

    assertEquals(srvc.statuses.map(_._1), Set(NotFound, Ok, Accepted).map(_.status))
  }

  test("A ResponseGenerator should match four results with different status but same result type") {
    val srvc = new TRhoRoutes[IO] {
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

    assertEquals(srvc.statuses.map(_._1), Set(NotFound, Ok, Accepted, Created).map(_.status))
  }

  test("A ResponseGenerator should match results with locally defined types") {
    case class ModelA(name: String, color: Int)
    case class ModelB(name: String, id: Long)

    implicit def w1[F[_]]: EntityEncoder[F, ModelA] =
      EntityEncoder.simple[ModelA]()(_ => Chunk.array("A".getBytes))

    implicit def w2[F[_]]: EntityEncoder[F, ModelB] =
      EntityEncoder.simple[ModelB]()(_ => Chunk.array("B".getBytes))

    val srvc = new TRhoRoutes[IO] {
      GET / "foo" |>> { () =>
        if (true) Ok(ModelA("test ok", 1))
        else NotFound(ModelB("test not found", 234))
      }
    }

    assertEquals(srvc.statuses.map(_._1), Set(Ok.status, NotFound.status))

    // the type equality for locally defined types is a bit "murkey" so we use the String name
    assertEquals(srvc.statuses.collect { case (HStatus.Ok, t) => t }.head.toString, "ModelA")
    assertEquals(srvc.statuses.collect { case (HStatus.NotFound, t) => t }.head.toString, "ModelB")
  }

  test("A ResponseGenerator should match complex models as well as simple ones") {
    import Foo._

    val srvc = new TRhoRoutes[IO] {
      GET / "foo" |>> { () =>
        if (true) Ok(FooA("test ok", 1))
        else NotFound(FooB("test not found", 234))
      }
    }

    assertEquals(srvc.statuses.map(_._1), Set(Ok.status, NotFound.status))
    assert(srvc.statuses.collect { case (HStatus.Ok, t) => t }.head =:= weakTypeOf[FooA])
    assert(
      srvc.statuses.collect { case (HStatus.NotFound, t) => t }.head
        =:= weakTypeOf[FooB]
    )
  }
}

object Foo {
  case class FooA(name: String, color: Int)
  case class FooB(name: String, id: Long)

  implicit def w1[F[_]]: EntityEncoder[F, FooA] =
    EntityEncoder.simple[FooA]()(_ => Chunk.array("A".getBytes))

  implicit def w2[F[_]]: EntityEncoder[F, FooB] =
    EntityEncoder.simple[FooB]()(_ => Chunk.array("B".getBytes))
}
