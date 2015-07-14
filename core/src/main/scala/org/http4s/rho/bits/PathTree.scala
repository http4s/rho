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

import scalaz.concurrent.Task

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

  def getResult(req: Request): RouteResult[ResponseAction] = paths.walkTree(req.method, req)
}

private[rho] object PathTree {

  type ResponseAction = () => Task[Response]

  def apply(): PathTree = new PathTree(new HeadNode())

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
          } yield () => route.action.act(req, j.asInstanceOf[T])
        }

      case c @ CodecRouter(_, parser) =>
        Leaf(c.router.query, c.headers, Some(parser)){ (req, pathstack) =>
          for {
            i <- ValidationTools.runQuery(req, c.router.query, pathstack)
            j <- ValidationTools.runValidation(req, c.headers, i)
          } yield () => parser.decode(req).run.flatMap(_.fold(e =>
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
    def apply(query: QueryRule, vals: HeaderRule, codec: Option[EntityDecoder[_]])
             (f: (Request, HList) => ParserResult[ResponseAction]): Leaf = SingleLeaf(f)
  }

  /** Leaves of the PathTree */
  private trait Leaf {
    /** Attempt to match this leaf */
    def attempt(req: Request, stack: HList): ParserResult[ResponseAction]

    /** Concatenate this leaf with another, giving the first precedence */
    final def ++(l: Leaf): Leaf = (this, l) match {
      case (s1@SingleLeaf(_), s2@SingleLeaf(_)) => ListLeaf(s1 :: s2 :: Nil)
      case (s1@SingleLeaf(_), ListLeaf(l)) => ListLeaf(s1 :: l)
      case (ListLeaf(l), s2@SingleLeaf(_)) => ListLeaf(l :+ s2)
      case (ListLeaf(l1), ListLeaf(l2)) => ListLeaf(l1 ::: l2)
    }
  }

  final private case class SingleLeaf(f: (Request, HList) => ParserResult[ResponseAction]) extends Leaf {
    override def attempt(req: Request, stack: HList): ParserResult[ResponseAction] = f(req, stack)
  }


  final private case class ListLeaf(leaves: List[SingleLeaf]) extends Leaf {
    override def attempt(req: Request, stack: HList): ParserResult[ResponseAction] = {
      @tailrec
      def go(l: List[SingleLeaf], error: ParserResult[Nothing]): ParserResult[ResponseAction] = if (l.nonEmpty) {
        l.head.attempt(req, stack) match {
          case r@ParserSuccess(_) => r
          case e@ParserFailure(_) => go(l.tail, if (error != null) error else e)
          case e@ValidationFailure(_) => go(l.tail, if (error != null) error else e)
        }
      } else if (error != null) error else sys.error("Leaf was empty!")
      go(leaves, null)
    }
  }

  /** Nodes that form the path tree */
  private abstract class Node {

    protected def paths: List[Node]

    protected def end: Map[Method, Leaf]

    protected def variadic: Map[Method, Leaf]

    protected def clone(paths: List[Node], variadic: Map[Method,Leaf], end: Map[Method,Leaf]): Node

    protected def addNode(n: Node): Node

    protected def replaceNode(o: Node, n: Node): Node

    protected def matchString(s: String, stack: HList): HList

    // Appends the action to the tree by walking the PathRule stack, returning a new Node structure
    final def append(tail: PathRule, method: Method, action: Leaf): Node = append(tail::Nil, method, action)

    final protected def append(tail: List[PathRule], method: Method, action: Leaf): Node = tail match {
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
    final def walkTree(method: Method, req: Request): RouteResult[ResponseAction] = {
      val path = keyToPath(req)
      walk(method, req, if (path.nonEmpty) path else ""::Nil, HNil)
    }

    def walk(method: Method, req: Request, path: List[String], stack: HList): RouteResult[ResponseAction] = {
      val h = matchString(path.head, stack)
      if (h != null) {
        if (path.tail.isEmpty) {
          end.get(method).map(_.attempt(req, h)) orElse
            variadic.get(method).map(_.attempt(req, Nil::h)) getOrElse {
            if (end.keys.isEmpty || method == Method.OPTIONS) NoMatch
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
          def go(nodes: List[Node], error: ParserResult[Nothing]): ParserResult[ResponseAction] = {
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

  /** Root node */
  private case class HeadNode(paths: List[Node] = Nil,
                              variadic: Map[Method,Leaf] = Map.empty,
                              end: Map[Method,Leaf] = Map.empty) extends Node {

    override def walk(method: Method, req: Request, path: List[String], stack: HList): RouteResult[ResponseAction] = {
      if (path.isEmpty) end.get(method) orElse variadic.get(method) match {
        case Some(f) => f.attempt(req, stack)
        case None    => NoMatch
      }
      else {
        @tailrec               // error may be null
        def go(ns: List[Node], error: ParserResult[Nothing]): RouteResult[ResponseAction] = ns match {
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

  /** Node that parses a segment */
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

  /** Node that matches a path segment */
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
