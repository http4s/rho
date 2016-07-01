package org.http4s
package rho.bits

import scala.language.higherKinds

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import org.http4s.EntityDecoder
import org.http4s.DecodeFailure
import scalaz.concurrent.Task

/** Extract a value from the `Request` `Query`
  *
  * @tparam A Type of value produced by the parser.
  */
 trait EntityParser[A] { self => 
  def consumes: Set[MediaRange]

  def parse(input: Message): Task[ResultResponse[A]]

  def orElse[B >: A](other: EntityParser[B]): EntityParser[B] = 
    new EntityParser[B] {
      def consumes = self.consumes ++ other.consumes
      def parse(input: Message): Task[ResultResponse[B]] = 
        self.parse(input).flatMap { 
          case SuccessResponse(v) => Task(SuccessResponse(v))
          case FailureResponse(_) => other.parse(input)
        }
    }
}

object EntityParser {
  def apply[A](implicit parser: EntityParser[A]): EntityParser[A] = parser

  object defaults {
    import scala.language.implicitConversions

    implicit def defaultEntityParserInstance[A](implicit entityDecoder: EntityDecoder[A]) = new EntityParser[A] {
      val consumes: Set[MediaRange] = entityDecoder.consumes

      def parse(input: Message): Task[ResultResponse[A]] = 
        entityDecoder.decode(input, false).fold(
          e => FailureResponse.pure(e.toHttpResponse(input.httpVersion)),
          SuccessResponse(_)
        )
    }
  }
}

