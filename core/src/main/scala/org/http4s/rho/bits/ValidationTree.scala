package org.http4s
package rho
package bits

import QueryAST.QueryRule
import HeaderAST.HeaderRule

import scala.annotation.tailrec

import shapeless.HList

import scalaz.concurrent.Task


private[bits] object TempTools extends ExecutableCompiler {
  override def runValidation(req: Request, v: HeaderRule, stack: HList): ParserResult[HList] =
    super.runValidation(req, v, stack)

  override def runQuery(req: Request, v: QueryRule, stack: HList): ParserResult[HList] =
    super.runQuery(req, v, stack)
}

private[rho] trait ValidationTree {

  private type Result = ParserResult[()=>Task[Response]]

  protected trait Leaf {
    def attempt(req: Request, stack: HList): Result

    final def ++(l: Leaf): Leaf = (this, l) match {
      case (s1@ SingleLeaf(_,_,_,_), s2@ SingleLeaf(_,_,_,_)) => ListLeaf(s1::s2::Nil)
      case (s1@ SingleLeaf(_,_,_,_), ListLeaf(l))             => ListLeaf(s1::l)
      case (ListLeaf(l), s2@ SingleLeaf(_,_,_,_))             => ListLeaf(l:+s2)
      case (ListLeaf(l1), ListLeaf(l2))                       => ListLeaf(l1:::l2)
    }
  }

  final private case class SingleLeaf(query: QueryRule,
                                      vals: HeaderRule,       // TODO: For documentation purposes
                                      codec: Option[EntityDecoder[_]],  // For documentation purposes
                                      f: (Request, HList) => Result) extends Leaf {
    override def attempt(req: Request, stack: HList): Result = f(req,stack)
  }


  final private case class ListLeaf(leaves: List[SingleLeaf]) extends Leaf {
    override def attempt(req: Request, stack: HList): Result = {
      @tailrec
      def go(l: List[SingleLeaf], error: ParserResult[Nothing]): Result = if (!l.isEmpty) {
        l.head.attempt(req, stack) match {
          case r@ ParserSuccess(_)      => r
          case e @ ParserFailure(_)     => go(l.tail, if (error != null) error else e)
          case e @ ValidationFailure(_) => go(l.tail, if (error != null) error else e)

        }
      } else error
      go(leaves, null)
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////
  protected def makeLeaf[T <: HList, F, O](action: RhoAction[T, F]): Leaf = {
    action.router match {
      case Router(method, _, query, vals) =>
        SingleLeaf(query, vals, None, (req, pathstack) => {
          for {
            i <- TempTools.runQuery(req, query, pathstack)
            j <- TempTools.runValidation(req, vals, i)
          } yield (() => action.hf.conv(action.f)(req, j.asInstanceOf[T]))
        })

      case c@ CodecRouter(_, parser) =>
        val actionf = action.hf.conv(action.f)
        SingleLeaf(c.router.query, c.validators, Some(parser), {
          (req, pathstack) =>
              for {
                i <- TempTools.runQuery(req, c.router.query, pathstack)
                j <- TempTools.runValidation(req, c.validators, i)
              } yield (() => {
                parser.decode(req).flatMap {
                  case ParserSuccess(r)     => actionf(req, (r :: pathstack).asInstanceOf[T])
                  case ParserFailure(e)     => TempTools.onBadRequest(s"Decoding error: $e")
                  case ValidationFailure(e) => TempTools.onBadRequest(s"Validation failure: $e")
                }
              })
        })
    }
  }

}
