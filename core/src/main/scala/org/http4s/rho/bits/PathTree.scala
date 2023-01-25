package org.http4s
package rho
package bits

import cats.{Applicative, Monad}
import cats.implicits._
import org.http4s.rho.bits.PathAST._
import org.log4s.getLogger
import shapeless.{HList, HNil}

import scala.annotation.tailrec
import scala.util.control.NonFatal

object PathTree {

  def apply[F[_]](): PathTreeOps[F]#PathTree = {
    val ops = new PathTreeOps[F] {}
    new ops.PathTree(ops.MatchNode(Uri.Path.Segment.empty))
  }

  def splitPath(path: Uri.Path): List[Uri.Path.Segment] =
    if (path.isEmpty) {
      List(Uri.Path.Segment.empty)
    } else {
      val noEmptySegments = path.segments.filterNot(_.isEmpty).toList
      if (path.endsWithSlash) {
        noEmptySegments :+ Uri.Path.Segment.empty
      } else {
        noEmptySegments
      }
    }
}

private[rho] trait PathTreeOps[F[_]] extends RuleExecutor[F] {
  /* To avoid alocating new NoMatch()s all the time. */
  private val noMatch: RouteResult[F, Nothing] = NoMatch[F]()

  /** Data structure for route execution
    *
    * A [[PathTree]] contains a map of the known route paths. The values of the
    * tree are [[RhoRoute]]'s that can generate a reply to a `Request`.
    */
  final class PathTree(private val paths: MatchNode) {

    override def toString: String = paths.toString

    /** Create a new [[PathTree]] with the [[RhoRoute]] appended
      *
      * @param route [[RhoRoute]] to append to the new tree.
      * @tparam T Result type of the [[RhoRoute]].
      * @return A new [[PathTree]] containing the provided route.
      */
    def appendRoute[T <: HList](route: RhoRoute[F, T])(implicit F: Monad[F]): PathTree = {
      val m = route.method
      val newLeaf = makeLeaf(route)
      val newNode = paths.append(route.path, m, newLeaf)
      new PathTree(newNode)
    }

    /** Merge this tree with another [[PathTree]] */
    def merge(other: PathTree): PathTree = new PathTree(paths.merge(other.paths))

    /** Attempt to generate a `Response` by executing the tree. */
    def getResult(req: Request[F])(implicit F: Monad[F]): RouteResult[F, F[Response[F]]] =
      paths.walkTree(req.method, req)
  }

  private val logger = getLogger

  type Action = ResultResponse[F, F[Response[F]]]

  def makeLeaf[T <: HList](route: RhoRoute[F, T])(implicit F: Monad[F]): Leaf =
    route.router match {
      case Router(_, _, rules) =>
        Leaf { (req, pathstack) =>
          runRequestRules(req, rules, pathstack).map { i =>
            route.action.act(req, i.asInstanceOf[T])
          }
        }

      case c @ CodecRouter(_, parser) =>
        Leaf { (req, pathstack) =>
          runRequestRules(req, c.router.rules, pathstack).map { i =>
            parser
              .decode(req, false)
              .foldF(
                _.toHttpResponse[F](req.httpVersion).pure[F],
                body =>
                  // `asInstanceOf` to turn the untyped HList to type T
                  route.action.act(req, (body :: i).asInstanceOf[T])
              )
          }
        }
    }

  //////////////////////////////////////////////////////

  object Leaf {
    def apply(f: (Request[F], HList) => Action)(implicit F: Applicative[F]): Leaf = SingleLeaf(f)
  }

  /** Leaves of the PathTree */
  sealed trait Leaf {

    /** Attempt to match this leaf */
    def attempt(req: Request[F], stack: HList): Action

    /** Concatenate this leaf with another, giving the first precedence */
    final def ++(l: Leaf): Leaf = (this, l) match {
      case (s1 @ SingleLeaf(_), s2 @ SingleLeaf(_)) => ListLeaf(s1 :: s2 :: Nil)
      case (s1 @ SingleLeaf(_), ListLeaf(l)) => ListLeaf(s1 :: l)
      case (ListLeaf(l), s2 @ SingleLeaf(_)) => ListLeaf(l :+ s2)
      case (ListLeaf(l1), ListLeaf(l2)) => ListLeaf(l1 ::: l2)
    }
  }

  private case class SingleLeaf(f: (Request[F], HList) => Action)(implicit F: Applicative[F])
      extends Leaf {
    override def attempt(req: Request[F], stack: HList): Action =
      try f(req, stack)
      catch {
        case NonFatal(t) =>
          logger.error(t)("Error in action execution")
          SuccessResponse(F.pure(Response(status = Status.InternalServerError)))
      }
  }

  private case class ListLeaf(leaves: List[SingleLeaf]) extends Leaf {
    override def attempt(req: Request[F], stack: HList): Action = {
      def go(l: List[SingleLeaf], error: ResultResponse[F, Nothing]): Action =
        if (l.nonEmpty) l.head.attempt(req, stack) match {
          case r @ SuccessResponse(_) => r
          case e @ FailureResponse(_) => go(l.tail, if (error != null) error else e)
        }
        else if (error != null) error
        else sys.error("Leaf was empty!")
      go(leaves, null)
    }
  }

  sealed trait Node[Self <: Node[Self]] extends ResponseGeneratorInstances[F] {

    def matches: Map[Uri.Path.Segment, MatchNode]

    def captures: List[CaptureNode]

    def variadic: Map[Method, Leaf]

    def end: Map[Method, Leaf]

    def clone(
        matches: Map[Uri.Path.Segment, MatchNode],
        captures: List[CaptureNode],
        variadic: Map[Method, Leaf],
        end: Map[Method, Leaf]): Self

    def merge(other: Self): Self

    final def append(tail: PathRule, method: Method, action: Leaf): Self =
      append(tail :: Nil, method, action)

    // Add elements to the tree
    final protected def append(tail: List[PathRule], method: Method, action: Leaf): Self =
      tail match {
        case h :: t =>
          h match {
            case PathAnd(p1, p2) => append(p1 :: p2 :: t, method, action)

            case PathOr(p1, p2) => append(p1 :: t, method, action).append(p2 :: t, method, action)

            case MetaCons(r, _) => append(r :: t, method, action) // discard metadata

            case PathMatch.empty if !t.isEmpty =>
              append(t, method, action) // "" is a NOOP in the middle of a path

            // the rest of the types need to rewrite a node
            case PathMatch(s) =>
              val next = matches.getOrElse(s, MatchNode(s)).append(t, method, action)
              clone(matches.updated(s, next), captures, variadic, end)

            case PathCapture(_, _, p, _) =>
              val exists = captures.exists { case CaptureNode(p1, _, _, _, _) => p1 eq p }
              val all = if (exists) captures.map {
                case n @ CaptureNode(p1, _, _, _, _) if p1 eq p => n.append(t, method, action)
                case n => n
              }
              else
                CaptureNode(p.asInstanceOf[StringParser[F, String]])
                  .append(t, method, action) :: captures

              clone(matches, all, variadic, end)

            case CaptureTail =>
              val l = variadic.get(method).map(_ ++ action).getOrElse(action)
              val v = variadic.updated(method, l)
              clone(matches, captures, v, end)
          }

        case Nil => // this is the end of the stack
          val l = end.get(method).map(_ ++ action).getOrElse(action)
          val e = end.updated(method, l)
          clone(matches, captures, variadic, e)
      }

    /** This function traverses the tree, matching paths in order of priority, provided they path the matches function:
      * 1: exact matches are given priority to wild cards node at a time
      *     This means /"foo"/wild has priority over /wild/"bar" for the route "/foo/bar"
      */
    final def walkTree(method: Method, req: Request[F])(implicit
        F: Monad[F]): RouteResult[F, F[Response[F]]] = {
      val path = PathTree.splitPath(req.pathInfo)
      walk(method, req, path, HNil)
    }

    // This should scan all forward paths.
    final protected def walk(
        method: Method,
        req: Request[F],
        path: List[Uri.Path.Segment],
        stack: HList)(implicit F: Monad[F]): RouteResult[F, F[Response[F]]] = {
      def tryVariadic(result: RouteResult[F, F[Response[F]]]): RouteResult[F, F[Response[F]]] =
        variadic.get(method) match {
          case None => result
          case Some(l) => l.attempt(req, path :: stack)
        }

      def walkHeadTail(
          h: Uri.Path.Segment,
          t: List[Uri.Path.Segment]): RouteResult[F, F[Response[F]]] = {
        val exact: RouteResult[F, F[Response[F]]] = matches.get(h) match {
          case Some(n) => n.walk(method, req, t, stack)
          case None => noMatch
        }

        @tailrec
        def go(
            children: List[CaptureNode],
            error: RouteResult[F, F[Response[F]]]): RouteResult[F, F[Response[F]]] =
          children match {
            case (c @ CaptureNode(p, _, _, _, _)) :: ns =>
              p.parse(h.decoded()) match { //TODO: Figure out how to inject char sets etc...
                case SuccessResponse(r) =>
                  val n = c.walk(method, req, t, r :: stack)
                  if (n.isSuccess) n
                  else if (error.isEmpty) go(ns, n)
                  else go(ns, error)

                case r @ FailureResponse(_) =>
                  go(ns, if (error.isEmpty) r.asInstanceOf[FailureResponse[F]] else error)
              }

            case Nil => tryVariadic(error)
          }

        if (exact.isSuccess) exact
        else go(captures, exact)
      }

      def walkNil(): RouteResult[F, F[Response[F]]] =
        end.get(method) match {
          case Some(l) => l.attempt(req, stack)
          case None =>
            val result = tryVariadic(noMatch)

            if (!result.isEmpty || end.isEmpty || method == Method.OPTIONS) result
            else
              FailureResponse.pure[F] {
                val ms = end.keySet
                val allowedMethods = ms.mkString(", ")
                val msg = s"$method not allowed. Defined methods: $allowedMethods\n"

                F.map(MethodNotAllowed.pure(msg))(_.putHeaders(headers.Allow(ms)))
              }
        }

      path match {
        case h :: t => walkHeadTail(h, t)
        case Nil => walkNil()
      }
    }
  }

  case class MatchNode(
      name: Uri.Path.Segment,
      matches: Map[Uri.Path.Segment, MatchNode] = Map.empty[Uri.Path.Segment, MatchNode],
      captures: List[CaptureNode] = Nil,
      variadic: Map[Method, Leaf] = Map.empty[Method, Leaf],
      end: Map[Method, Leaf] = Map.empty[Method, Leaf])
      extends Node[MatchNode] {

    override def clone(
        matches: Map[Uri.Path.Segment, MatchNode],
        captures: List[CaptureNode],
        variadic: Map[Method, Leaf],
        end: Map[Method, Leaf]): MatchNode =
      copy(matches = matches, captures = captures, variadic = variadic, end = end)

    override def merge(other: MatchNode): MatchNode = {
      require(
        other.name == name,
        s"${this.getClass.getSimpleName}s don't have the same name, this: $name, that: ${other.name}."
      )

      clone(
        mergeMatches(matches, other.matches),
        mergeCaptures(captures, other.captures),
        mergeLeaves(variadic, other.variadic),
        mergeLeaves(end, other.end)
      )
    }
  }

  case class CaptureNode(
      parser: StringParser[F, _],
      matches: Map[Uri.Path.Segment, MatchNode] = Map.empty[Uri.Path.Segment, MatchNode],
      captures: List[CaptureNode] = List.empty[CaptureNode],
      variadic: Map[Method, Leaf] = Map.empty[Method, Leaf],
      end: Map[Method, Leaf] = Map.empty[Method, Leaf])
      extends Node[CaptureNode] {

    override def clone(
        matches: Map[Uri.Path.Segment, MatchNode],
        captures: List[CaptureNode],
        variadic: Map[Method, Leaf],
        end: Map[Method, Leaf]): CaptureNode =
      copy(matches = matches, captures = captures, variadic = variadic, end = end)

    override def merge(other: CaptureNode): CaptureNode = {
      require(
        other.parser eq parser,
        s"Cannot merge ${this.getClass.getSimpleName}s that have different parsers"
      )

      clone(
        mergeMatches(matches, other.matches),
        mergeCaptures(captures, other.captures),
        mergeLeaves(variadic, other.variadic),
        mergeLeaves(end, other.end)
      )
    }
  }

  // This ordering can get a little funky here. It is defined such that nodes in c2 get promoted in order if
  // they are also found in c1.
  // TODO: should we just concat the lists and be done with it? That would make captures a linear search...
  private def mergeCaptures(c1: List[CaptureNode], c2: List[CaptureNode]): List[CaptureNode] = {
    val first = c1.map(c => c2.find(_.parser eq c.parser).map(c merge _).getOrElse(c))
    val last = c2.filterNot(c => c1.exists(_.parser eq c.parser))
    first ++ last
  }

  private def mergeMatches(
      m1: Map[Uri.Path.Segment, MatchNode],
      m2: Map[Uri.Path.Segment, MatchNode]): Map[Uri.Path.Segment, MatchNode] =
    mergeMaps(m1, m2)(_ merge _)

  private def mergeLeaves(l1: Map[Method, Leaf], l2: Map[Method, Leaf]): Map[Method, Leaf] =
    mergeMaps(l1, l2)(_ ++ _)

  private def mergeMaps[K, V](m1: Map[K, V], m2: Map[K, V])(merge: (V, V) => V): Map[K, V] = {
    val allMatches = m1.keySet ++ m2.keySet
    allMatches.toSeq.map { k =>
      (m1.get(k), m2.get(k)) match {
        case (Some(l1), Some(l2)) => (k, merge(l1, l2))
        case (Some(l1), None) => (k, l1)
        case (None, Some(l2)) => (k, l2)
        case (None, None) =>
          sys.error("Shouldn't get here: each name should be defined in at least one of the maps")
      }
    }.toMap
  }
}
