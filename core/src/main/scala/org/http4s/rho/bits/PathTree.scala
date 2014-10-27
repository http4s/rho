package org.http4s
package rho
package bits

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
  

  sealed protected trait Leaf {
    def attempt(req: Key, stack: HList): ParserResult[Value]

    final def ++(l: Leaf): Leaf = (this, l) match {
      case (s1@SingleLeaf(_, _, _, _), s2@SingleLeaf(_, _, _, _)) => ListLeaf(s1 :: s2 :: Nil)
      case (s1@SingleLeaf(_, _, _, _), ListLeaf(l)) => ListLeaf(s1 :: l)
      case (ListLeaf(l), s2@SingleLeaf(_, _, _, _)) => ListLeaf(l :+ s2)
      case (ListLeaf(l1), ListLeaf(l2)) => ListLeaf(l1 ::: l2)
    }
  }

  final protected case class SingleLeaf(query: QueryRule,
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

    protected def end: Leaf

    protected def variadic: Leaf

    protected def clone(paths: List[Node], variadic: Leaf, end: Leaf): Node

    protected def addNode(n: Node): Node

    protected def replaceNode(o: Node, n: Node): Node

    protected def matchString(s: String, stack: HList): HList

    // Appends the action to the tree by walking the PathRule stack, returning a new Node structure
    final def append(tail: PathRule, action: Leaf): Node = append(tail::Nil, action)

    final private[Node] def append(tail: List[PathRule], action: Leaf): Node = tail match {
      case h::tail => h match {
        case PathAnd(p1, p2) => append(p1::p2::tail, action)

        case PathOr(p1, p2) => append(p1::tail, action).append(p2::tail, action)

        case MetaCons(r, _) => append(r::tail, action)  // discard metadata

        case PathMatch("") if !tail.isEmpty => append(tail, action) // "" is a NOOP in the middle of a path

        case PathMatch(s) =>
          paths.collectFirst { case n@MatchNode(s1,_,_,_) if s == s1 => n } match {
            case Some(n) => replaceNode(n, n.append(tail, action))
            case None    => addNode(MatchNode(s).append(tail, action))
          }

        case PathCapture(_, p, _) =>
          paths.collectFirst{ case n@ CaptureNode(p1,_,_,_) if p1 eq p => n } match {
            case Some(w) => replaceNode(w, w.append(tail, action))
            case None    => addNode(CaptureNode(p).append(tail, action))
          }

        case CaptureTail() =>
          val v = if (variadic != null) variadic ++ action else action
          clone(paths, v, end)

        case PathEmpty => append(tail, action)
      }

      case Nil =>  // this is the end of the stack
        val e = if (end != null) end ++ action else action
        clone(paths, variadic, e)
    }
    
    /** This function traverses the tree, matching paths in order of priority, provided they path the matches function:
      * 1: exact matches are given priority to wild cards node at a time
      *     This means /"foo"/wild has priority over /wild/"bar" for the route "/foo/bar"
      */
    final def walkTree(req: Key): RouteResult[Value] = {
      val path = keyToPath(req)
      walk(req, if (path.nonEmpty) path else ""::Nil, HNil)
    }

    private[PathTree] def walk(req: Key, path: List[String], stack: HList): RouteResult[Value] = {
      val h = matchString(path.head, stack)
      if (h != null) {
        if (path.tail.isEmpty) {
          if (end != null) end.attempt(req, h)
          else if (variadic != null) variadic.attempt(req, Nil::h)
          else NoMatch
        }
        else {
          @tailrec               // error may be null
          def go(nodes: List[Node], error: ParserResult[Nothing]): ParserResult[Value] = {
            if (nodes.isEmpty) error
            else nodes.head.walk(req, path.tail, h) match {
              case NoMatch                 => go(nodes.tail, error)
              case r@ ParserSuccess(_)     => r
              case e@ ParserFailure(_)     => go(nodes.tail, if (error != null) error else e)
              case e@ ValidationFailure(_) => go(nodes.tail, if (error != null) error else e)
            }
          }

          val routeMatch = go(paths, null)
          if (routeMatch != null) routeMatch
          else if(variadic != null) variadic.attempt(req, path.tail::h)
          else NoMatch
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
                                variadic: Leaf = null,
                                end: Leaf = null) extends Node {

    private[PathTree] override def walk(req: Key, path: List[String], stack: HList): RouteResult[Value] = {
      if (path.isEmpty) {
        if (end != null) end.attempt(req, stack)
        else if (variadic != null) variadic.attempt(req, Nil::stack)
        else NoMatch
      }
      else {
        @tailrec               // error may be null
        def go(ns: List[Node], error: ParserResult[Nothing]): RouteResult[Value] = ns match {
          case Nil   => error
          case n::ns => n.walk(req, path, stack) match {
            case NoMatch                 => go(ns, error)
            case r@ ParserSuccess(_)     => r
            case e@ ParserFailure(_)     => go(ns, if (error != null) error else e)
            case e@ ValidationFailure(_) => go(ns, if (error != null) error else e)
          }
        }

        val routeMatch = go(paths, null)
        if (routeMatch != null) routeMatch
        else if(variadic != null) variadic.attempt(req, path.tail::stack)
        else NoMatch
      }
    }

    override protected def replaceNode(o: Node, n: Node): HeadNode = copy(paths = replace(o, n))

    override protected def addNode(n: Node): HeadNode = n match {
      case n: CaptureNode => copy(paths = paths:+n)
      case n: MatchNode   => copy(paths = n::paths)
      case n: HeadNode    => sys.error("Shouldn't get here!")
    }

    override protected def clone(paths: List[Node], variadic: Leaf, end: Leaf): HeadNode =
      copy(paths = paths, variadic = variadic, end = end)

    override protected def matchString(s: String, stack: HList): HList = {
      if (s.length == 0) stack
      else sys.error("Invalid start string")
    }
  }

  final private case class CaptureNode(parser: StringParser[_],
                                       paths: List[Node] = Nil,
                                       variadic: Leaf = null,
                                       end: Leaf = null
                                        ) extends Node {


    override protected def replaceNode(o: Node, n: Node): CaptureNode = copy(paths = replace(o, n))

    override protected def addNode(n: Node): CaptureNode = n match {
      case n: CaptureNode => copy(paths = paths:+n)
      case n: MatchNode   => copy(paths = n::paths)
      case n: HeadNode    => sys.error("Shouldn't get here!")
    }

    override protected def clone(paths: List[Node], variadic: Leaf, end: Leaf): CaptureNode =
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
                                     variadic: Leaf = null,
                                     end: Leaf = null) extends Node {

    override protected def replaceNode(o: Node, n: Node): MatchNode = copy(paths = replace(o, n))

    override protected def addNode(n: Node): MatchNode = n match {
      case n: CaptureNode => copy(paths = paths:+n)
      case n: MatchNode   => copy(paths = n::paths)
      case n: HeadNode    => sys.error("Shouldn't get here!")
    }

    override protected def clone(paths: List[Node], variadic: Leaf, end: Leaf): MatchNode =
      copy(paths = paths, variadic = variadic, end = end)

    override protected def matchString(s: String, h: HList): HList = if (s == name) h else null
  }
}

