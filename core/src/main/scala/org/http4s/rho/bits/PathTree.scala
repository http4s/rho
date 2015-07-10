package org.http4s
package rho
package bits

import scala.language.existentials

import org.http4s.rho.bits.HeaderAST.HeaderRule
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.QueryAST.QueryRule
import org.http4s.rho.bits.ResponseGeneratorInstances._

import shapeless.{HNil, HList}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

import scalaz.concurrent.Task

import org.log4s.getLogger

/** Rho specific implementation of the PathTree */
final class PathTree private(paths: PathTree.Node) {
  import PathTree._

  override def toString = paths.toString()

  def appendRoute[T <: HList](route: RhoRoute[T]): PathTree = {
    val m = route.method
    val newLeaf = makeLeaf(route)
    val newNode = paths.append(route.path, m, newLeaf)
    new PathTree(newNode)
  }


  def getResult(req: Request): RouteResult[Task[Response]] = paths.walkTree(req.method, req)
}

private[rho] object PathTree {


  private val logger = getLogger

  type Action = ParserResult[Task[Response]]

  def apply(): PathTree = new PathTree(MatchNode(""))

  object ValidationTools extends ExecutableCompiler

  def splitPath(path: String): List[String] = {
    val buff = new ListBuffer[String]
    val len = path.length
    @tailrec
    def go(i: Int, begin: Int): Unit = if (i < len) {
      if (path.charAt(i) == '/') {
        if (i > begin) buff += path.substring(begin, i)
        go(i + 1, i + 1)
      } else go(i + 1, begin)
    } else {
      buff += path.substring(begin, i)
    }

    val i = if (path.nonEmpty && path.charAt(0) == '/') 1 else 0
    go(i, i)

    buff.result
  }

  /** Generates a list of tokens that represent the path */
  private def keyToPath(key: Request): List[String] = splitPath(key.pathInfo)

  private def makeLeaf[T <: HList](route: RhoRoute[T]): Leaf = {
    route.router match {
      case Router(method, _, query, vals) =>
        Leaf(query, vals, None){ (req, pathstack) =>
          for {
            i <- ValidationTools.runQuery(req, query, pathstack)
            j <- ValidationTools.runValidation(req, vals, i) // `asInstanceOf` to turn the untyped HList to type T
          } yield route.action.act(req, j.asInstanceOf[T])
        }

      case c @ CodecRouter(_, parser) =>
        Leaf(c.router.query, c.headers, Some(parser)){ (req, pathstack) =>
          for {
            i <- ValidationTools.runQuery(req, c.router.query, pathstack)
            j <- ValidationTools.runValidation(req, c.headers, i)
          } yield parser.decode(req).run.flatMap(_.fold(e =>
            Response(Status.BadRequest, req.httpVersion).withBody(e.sanitized),
          { body =>
            // `asInstanceOf` to turn the untyped HList to type T
            route.action.act(req, (body :: j).asInstanceOf[T])
          }))
        }
    }
  }

  //////////////////////////////////////////////////////

  private object Leaf {
    def apply(query: QueryRule, vals: HeaderRule, codec: Option[EntityDecoder[_]])(f: (Request, HList) => Action): Leaf = SingleLeaf(f)
  }

  /** Leaves of the PathTree */
  private trait Leaf {
    /** Attempt to match this leaf */
    def attempt(req: Request, stack: HList): Action

    /** Concatenate this leaf with another, giving the first precedence */
    final def ++(l: Leaf): Leaf = (this, l) match {
      case (s1@SingleLeaf(_), s2@SingleLeaf(_)) => ListLeaf(s1 :: s2 :: Nil)
      case (s1@SingleLeaf(_), ListLeaf(l)) => ListLeaf(s1 :: l)
      case (ListLeaf(l), s2@SingleLeaf(_)) => ListLeaf(l :+ s2)
      case (ListLeaf(l1), ListLeaf(l2)) => ListLeaf(l1 ::: l2)
    }
  }

  final private case class SingleLeaf(f: (Request, HList) => Action) extends Leaf {
    override def attempt(req: Request, stack: HList): Action = {
      try f(req, stack)
      catch { case NonFatal(t) =>
        logger.error(t)("Error in action execution")
        ParserSuccess(Task.now(Response(status = Status.InternalServerError)))
      }
    }
  }


  final private case class ListLeaf(leaves: List[SingleLeaf]) extends Leaf {
    override def attempt(req: Request, stack: HList): Action = {
      def go(l: List[SingleLeaf], error: ParserResult[Nothing]): Action = {
        if (l.nonEmpty) l.head.attempt(req, stack) match {
          case r@ParserSuccess(_)     => r
          case e@ParserFailure(_)     => go(l.tail, if (error != null) error else e)
          case e@ValidationFailure(_) => go(l.tail, if (error != null) error else e)
        }
        else if (error != null) error else sys.error("Leaf was empty!")
      }
      go(leaves, null)
    }
  }

  private trait Node {

    type Self <: Node

    def matches: Map[String, MatchNode]

    def captures: List[CaptureNode]

    def variadic: Map[Method,Leaf]

    def end: Map[Method,Leaf]

    def clone(matches: Map[String, MatchNode], captures: List[CaptureNode], variadic: Map[Method,Leaf], end: Map[Method,Leaf]): Self

    final def append(tail: PathRule, method: Method, action: Leaf): Node = append(tail::Nil, method, action)

    // Add elements to the tree
    final protected def append(tail: List[PathRule], method: Method, action: Leaf): Self = tail match {
      case h::tail => h match {
        case PathAnd(p1, p2) => append(p1::p2::tail, method, action)

        case PathOr(p1, p2) => append(p1::tail, method, action).append(p2::tail, method, action).asInstanceOf[Self]

        case MetaCons(r, _) => append(r::tail, method, action)  // discard metadata

        case PathMatch("") if !tail.isEmpty => append(tail, method, action) // "" is a NOOP in the middle of a path

          // the rest of the types need to rewrite a node
        case PathMatch(s) =>
          val next = matches.getOrElse(s, MatchNode(s)).append(tail, method, action)
          clone(matches.updated(s, next), captures, variadic, end)

        case PathCapture(_, p, _) =>
          val exists = captures.exists{ case CaptureNode(p1,_,_,_,_) => p1 eq p }
          val all = if (exists) captures.map {
            case n@ CaptureNode(p1,_,_,_,_) if p1 eq p => n.append(tail, method, action)
            case n => n
          }
          else CaptureNode(p).append(tail, method, action)::captures

          clone(matches, all, variadic, end)

        case CaptureTail =>
          val l = variadic.get(method).map(_ ++ action).getOrElse(action)
          val v = variadic.updated(method, l)
          clone(matches, captures, v, end)
      }

      case Nil =>  // this is the end of the stack
        val l = end.get(method).map(_ ++ action).getOrElse(action)
        val e = end.updated(method, l)
        clone(matches, captures, variadic, e)
    }


    /** This function traverses the tree, matching paths in order of priority, provided they path the matches function:
      * 1: exact matches are given priority to wild cards node at a time
      *     This means /"foo"/wild has priority over /wild/"bar" for the route "/foo/bar"
      */
    final def walkTree(method: Method, req: Request): RouteResult[Task[Response]] = {
      val path = keyToPath(req)
      walk(method, req, path, HNil)
    }

    // This should scan all forward paths.
    final protected def walk(method: Method, req: Request, path: List[String], stack: HList): RouteResult[Task[Response]] = {
      def tryVariadic(result: RouteResult[Task[Response]]): RouteResult[Task[Response]] =
        variadic.get(method) match {
          case None    => result
          case Some(l) => l.attempt(req, path::stack)
        }

      path match {
        case h::t =>
          val exact = matches.get(h) match {
            case Some(n) => n.walk(method, req, t, stack)
            case None    => NoMatch
          }

          @tailrec
          def go(children: List[CaptureNode], error: RouteResult[Task[Response]]): RouteResult[Task[Response]] = children match {
              case (c@CaptureNode(p,_,cs,_,_))::ns =>
                p.parse(h) match {
                  case ParserSuccess(r) =>
                    val n = c.walk(method, req, t, r::stack)
                    if (n.isSuccess)        n
                    else if (error.isEmpty) go(ns, n)
                    else                    go(ns, error)

                  case r@ParserFailure(_)     if error.isEmpty => go(ns, r)
                  case r@ValidationFailure(_) if error.isEmpty => go(ns, r)
                  case _                                       => go(ns, error)
                }

              case Nil => tryVariadic(error)// try the variadiac
          }

          if (exact.isSuccess) exact
          else go(captures, exact)

        case Nil =>
          end.get(method) match {
            case Some(l) => l.attempt(req, stack)
            case None =>
              val result = tryVariadic(NoMatch)
              if (!result.isEmpty || end.keys.isEmpty || method == Method.OPTIONS) result
              else {
                val ms = end.keys
                val allowedMethods = ms.mkString(", ")
                val msg = s"$method not allowed. Defined methods: $allowedMethods\n"
                ValidationFailure.result(MethodNotAllowed(msg)
                  .withHeaders(headers.Allow(ms.head, ms.tail.toList:_*)))
              }
          }
      }
    }
  }

  private case class MatchNode(name:    String,
                              matches:  Map[String, MatchNode] = Map.empty,
                              captures: List[CaptureNode] = Nil,
                              variadic: Map[Method,Leaf] = Map.empty,
                              end:      Map[Method,Leaf] = Map.empty) extends Node {
    type Self = MatchNode
    override def clone(matches: Map[String, MatchNode], captures: List[CaptureNode], variadic: Map[Method, Leaf], end: Map[Method, Leaf]): Self =
      copy(matches = matches, captures = captures, variadic = variadic, end = end)
  }

  private case class CaptureNode(parser: StringParser[_],
                                 matches:  Map[String, MatchNode] = Map.empty,
                                 captures: List[CaptureNode] = Nil,
                                 variadic: Map[Method,Leaf] = Map.empty,
                                 end: Map[Method,Leaf] = Map.empty) extends Node {
    type Self = CaptureNode
    override def clone(matches: Map[String, MatchNode], captures: List[CaptureNode], variadic: Map[Method, Leaf], end: Map[Method, Leaf]): Self =
      copy(matches = matches, captures = captures, variadic = variadic, end = end)
  }
}
