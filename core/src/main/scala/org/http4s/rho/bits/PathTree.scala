package org.http4s
package rho
package bits

import org.http4s.rho.bits.ResponseGeneratorInstances.MethodNotAllowed

import scala.language.existentials

import org.http4s.rho.bits.PathAST._

import scala.annotation.tailrec

import QueryAST.QueryRule
import HeaderAST.HeaderRule

import shapeless.{HNil, HList}
import scala.collection.mutable.ListBuffer


/** Provides the machinery for walking a path tree */
trait PathTree {

  /** The element that will designate the route through the tree */
  type Key

  /** The result of a successful route */
  type Value

  /** Generates a list of tokens that represent the path */
  def keyToPath(key: Key): List[String]

  object Leaf {
    def apply(query: QueryRule, vals: HeaderRule, codec: Option[EntityDecoder[_]])
                      (f: (Key, HList) => ParserResult[Value]): Leaf = SingleLeaf(query, vals, codec, f)
  }

  /** Leaves of the PathTree */
  sealed trait Leaf {
    /** Attempt to match this leaf */
    def attempt(req: Key, stack: HList): ParserResult[Value]

    /** Concatenate this leaf with another, giving the first precedence */
    final def ++(l: Leaf): Leaf = (this, l) match {
      case (s1@SingleLeaf(_, _, _, _), s2@SingleLeaf(_, _, _, _)) => ListLeaf(s1 :: s2 :: Nil)
      case (s1@SingleLeaf(_, _, _, _), ListLeaf(l)) => ListLeaf(s1 :: l)
      case (ListLeaf(l), s2@SingleLeaf(_, _, _, _)) => ListLeaf(l :+ s2)
      case (ListLeaf(l1), ListLeaf(l2)) => ListLeaf(l1 ::: l2)
    }
  }

  final private case class SingleLeaf(query: QueryRule,
                                       vals: HeaderRule, // TODO: For documentation purposes
                                      codec: Option[EntityDecoder[_]], // For documentation purposes
                                          f: (Key, HList) => ParserResult[Value]) extends Leaf {
    override def attempt(req: Key, stack: HList): ParserResult[Value] = f(req, stack)
  }


  final private case class ListLeaf(leaves: List[SingleLeaf]) extends Leaf {
    override def attempt(req: Key, stack: HList): ParserResult[Value] = {
      @tailrec
      def go(l: List[SingleLeaf], error: ParserResult[Nothing]): ParserResult[Value] = if (l.nonEmpty) {
        l.head.attempt(req, stack) match {
          case r@ParserSuccess(_) => r
          case e@ParserFailure(_) => go(l.tail, if (error != null) error else e)
          case e@ValidationFailure(_) => go(l.tail, if (error != null) error else e)
        }
      } else if (error != null) error else sys.error("Leaf was empty!")
      go(leaves, null)
    }
  }

  protected sealed abstract class Node {

    protected def paths: List[Node]

    protected def end: Map[Method, Leaf]

    protected def variadic: Map[Method, Leaf]

    protected def clone(paths: List[Node], variadic: Map[Method,Leaf], end: Map[Method,Leaf]): Node

    protected def addNode(n: Node): Node

    protected def replaceNode(o: Node, n: Node): Node

    protected def matchString(s: String, stack: HList): HList

    // Appends the action to the tree by walking the PathRule stack, returning a new Node structure
    final def append(tail: PathRule, method: Method, action: Leaf): Node = append(tail::Nil, method, action)

    final private[Node] def append(tail: List[PathRule], method: Method, action: Leaf): Node = tail match {
      case h::tail => h match {
        case PathAnd(p1, p2) => append(p1::p2::tail, method, action)

        case PathOr(p1, p2) => append(p1::tail, method, action).append(p2::tail, method, action)

        case MetaCons(r, _) => append(r::tail, method, action)  // discard metadata

        case PathMatch("") if !tail.isEmpty => append(tail, method, action) // "" is a NOOP in the middle of a path

        case PathMatch(s) =>
          paths.collectFirst { case n@MatchNode(s1,_,_,_) if s == s1 => n } match {
            case Some(n) => replaceNode(n, n.append(tail, method, action))
            case None    => addNode(MatchNode(s).append(tail, method, action))
          }

        case PathCapture(_, p, _) =>
          paths.collectFirst{ case n@ CaptureNode(p1,_,_,_) if p1 eq p => n } match {
            case Some(w) => replaceNode(w, w.append(tail, method, action))
            case None    => addNode(CaptureNode(p).append(tail, method, action))
          }

        case CaptureTail =>
          val l = variadic.get(method).map(_ ++ action).getOrElse(action)
          val v = variadic.updated(method, l)
          clone(paths, v, end)
      }

      case Nil =>  // this is the end of the stack
        val l = end.get(method).map(_ ++ action).getOrElse(action)
        val e = end.updated(method, l)
        clone(paths, variadic, e)
    }
    
    /** This function traverses the tree, matching paths in order of priority, provided they path the matches function:
      * 1: exact matches are given priority to wild cards node at a time
      *     This means /"foo"/wild has priority over /wild/"bar" for the route "/foo/bar"
      */
    final def walkTree(method: Method, req: Key): RouteResult[Value] = {
      val path = keyToPath(req)
      walk(method, req, if (path.nonEmpty) path else ""::Nil, HNil)
    }

    private[PathTree] def walk(method: Method, req: Key, path: List[String], stack: HList): RouteResult[Value] = {
      val h = matchString(path.head, stack)
      if (h != null) {
        if (path.tail.isEmpty) {
          end.get(method).map(_.attempt(req, h)) orElse
            variadic.get(method).map(_.attempt(req, Nil::h)) getOrElse {
              if (end.keys.isEmpty) NoMatch
              else {
                val ms = end.keys
                val allowedMethods = ms.mkString(", ")
                val msg = s"$method not allowed. Defined methods: $allowedMethods\n"
                ValidationFailure(MethodNotAllowed(msg)
                  .withHeaders(headers.Allow(ms.head, ms.tail.toList:_*)))
              }
            }
        }
        else {
          @tailrec             // warning: `error` may be null
          def go(nodes: List[Node], error: ParserResult[Nothing]): ParserResult[Value] = {
            if (nodes.isEmpty) error
            else nodes.head.walk(method, req, path.tail, h) match {
              case NoMatch                 => go(nodes.tail, error)
              case r@ ParserSuccess(_)     => r
              case e@ ParserFailure(_)     => go(nodes.tail, if (error != null) error else e)
              case e@ ValidationFailure(_) => go(nodes.tail, if (error != null) error else e)
            }
          }

          val routeMatch = go(paths, null)
          if (routeMatch != null) routeMatch
          else variadic.get(method).map(_.attempt(req, path.tail::h)).getOrElse(NoMatch)
        }

      }
      else NoMatch
    }

    // Searches the available nodes and replaces the current one
    protected def replace(o: Node, n: Node): List[Node] = {
      val b = new ListBuffer[Node]
      def go(l: List[Node]): List[Node] = l match {
        case h::tail if h eq o => b += n; b.prependToList(tail)
        case h::tail           => b += h; go(tail)
        case _                 => sys.error("Shouldn't get here!")
      }
      go(paths)
    }
  }

  protected case class HeadNode(paths: List[Node] = Nil,
                             variadic: Map[Method,Leaf] = Map.empty,
                                  end: Map[Method,Leaf] = Map.empty) extends Node {

    private[PathTree] override def walk(method: Method, req: Key, path: List[String], stack: HList): RouteResult[Value] = {
      if (path.isEmpty) end.get(method) orElse variadic.get(method) match {
        case Some(f) => f.attempt(req, stack)
        case None    => NoMatch
      }
      else {
        @tailrec               // error may be null
        def go(ns: List[Node], error: ParserResult[Nothing]): RouteResult[Value] = ns match {
          case Nil   => error
          case n::ns => n.walk(method, req, path, stack) match {
            case NoMatch                 => go(ns, error)
            case r@ ParserSuccess(_)     => r
            case e@ ParserFailure(_)     => go(ns, if (error != null) error else e)
            case e@ ValidationFailure(_) => go(ns, if (error != null) error else e)
          }
        }

        val routeMatch = go(paths, null)
        if (routeMatch != null) routeMatch
        else variadic.get(method) match {
          case Some(f) => f.attempt(req, path.tail::stack)
          case None    => NoMatch
        }
      }
    }

    override protected def replaceNode(o: Node, n: Node): HeadNode = copy(paths = replace(o, n))

    override protected def addNode(n: Node): HeadNode = n match {
      case n: CaptureNode => copy(paths = paths:+n)
      case n: MatchNode   => copy(paths = n::paths)
      case n: HeadNode    => sys.error("Shouldn't get here!")
    }

    override protected def clone(paths: List[Node], variadic: Map[Method,Leaf], end: Map[Method,Leaf]): HeadNode =
      copy(paths = paths, variadic = variadic, end = end)

    override protected def matchString(s: String, stack: HList): HList = {
      if (s.length == 0) stack
      else sys.error("Invalid start string")
    }
  }

  final private case class CaptureNode(parser: StringParser[_],
                                       paths: List[Node] = Nil,
                                       variadic: Map[Method,Leaf] = Map.empty,
                                       end: Map[Method,Leaf] = Map.empty) extends Node
  {


    override protected def replaceNode(o: Node, n: Node): CaptureNode = copy(paths = replace(o, n))

    override protected def addNode(n: Node): CaptureNode = n match {
      case n: CaptureNode => copy(paths = paths:+n)
      case n: MatchNode   => copy(paths = n::paths)
      case n: HeadNode    => sys.error("Shouldn't get here!")
    }

    override protected def clone(paths: List[Node], variadic: Map[Method,Leaf], end: Map[Method,Leaf]): CaptureNode =
      copy(paths = paths, variadic = variadic, end = end)

    override protected def matchString(s: String, h: HList): HList = {
      parser.parse(s) match {
        case ParserSuccess(v) => v::h
        case _ => null
      }
    }
  }

  final private case class MatchNode(name: String,
                                     paths: List[Node] = Nil,
                                     variadic: Map[Method,Leaf] = Map.empty,
                                     end: Map[Method,Leaf] = Map.empty) extends Node {

    override protected def replaceNode(o: Node, n: Node): MatchNode = copy(paths = replace(o, n))

    override protected def addNode(n: Node): MatchNode = n match {
      case n: CaptureNode => copy(paths = paths:+n)
      case n: MatchNode   => copy(paths = n::paths)
      case n: HeadNode    => sys.error("Shouldn't get here!")
    }

    override protected def clone(paths: List[Node], variadic: Map[Method,Leaf], end: Map[Method,Leaf]): MatchNode =
      copy(paths = paths, variadic = variadic, end = end)

    override protected def matchString(s: String, h: HList): HList = if (s == name) h else null
  }
}

