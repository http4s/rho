package org.http4s
package rho
package bits

import cats.Monad
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.ResponseGeneratorInstances._
import org.http4s.util.UrlCodingUtils
import org.log4s.getLogger
import shapeless.{HList, HNil}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.util.control.NonFatal

/** Data structure for route execution
  *
  * A [[PathTree]] contains a map of the known route paths. The values of the
  * tree are [[RhoRoute]]'s that can generate a reply to a `Request`.
  */
private[rho] final class PathTree[F[_]] private(private val paths: PathTree.MatchNode[F]) {
  import PathTree._

  override def toString = paths.toString()

  /** Create a new [[PathTree]] with the [[RhoRoute]] appended
    *
    * @param route [[RhoRoute]] to append to the new tree.
    * @tparam T Result type of the [[RhoRoute]].
    * @return A new [[PathTree]] containing the provided route.
    */
  def appendRoute[T <: HList](route: RhoRoute[F, T]): PathTree[F] = {
    val m = route.method
    val newLeaf = makeLeaf(route)
    val newNode = paths.append(route.path, m, newLeaf)
    new PathTree(newNode)
  }

  /** Merge this tree with another [[PathTree]] */
  def merge(other: PathTree[F]): PathTree[F] = new PathTree(paths merge other.paths)

  /** Attempt to generate a `Response` by executing the tree. */
  def getResult(req: Request[F])(implicit F: Monad[F]): RouteResult[F, Response[F]] = paths.walkTree(req.method, req)
}

private[rho] object PathTree {
  private val logger = getLogger

  type Action[F[_]] = ResultResponse[F, Response[F]]

  def apply[F[_]](): PathTree[F] = new PathTree[F](MatchNode[F](""))

  def splitPath(path: String): List[String] = {
    val buff = new ListBuffer[String]
    val len = path.length

    @tailrec
    def go(i: Int, begin: Int): Unit = {
      if (i < len) {
        if (path.charAt(i) == '/') {
          if (i > begin) buff += UrlCodingUtils.urlDecode(path.substring(begin, i))
          go(i + 1, i + 1)
        } else go(i + 1, begin)
      } else {
        buff += UrlCodingUtils.urlDecode(path.substring(begin, i))
      }
    }

    val i = if (path.nonEmpty && path.charAt(0) == '/') 1 else 0
    go(i, i)

    buff.result
  }

  /** Generates a list of tokens that represent the path */
  private def keyToPath[F[_]](key: Request[F]): List[String] = splitPath(key.pathInfo)

  private def makeLeaf[F[_], T <: HList](route: RhoRoute[F, T]): Leaf[F] = {
    route.router match {
      case Router(method, _, rules) =>
        Leaf[F] { (req, pathstack) =>
          RuleExecutor.runRequestRules(req, rules, pathstack).map { i =>
            ??? // TODO: impl
//            route.action.act(req, i.asInstanceOf[T])
          }
        }

      case c @ CodecRouter(_, parser) =>
        Leaf[F] { (req, pathstack) =>
          RuleExecutor.runRequestRules(req, c.router.rules, pathstack).map { i =>
            ??? // TODO: impl
//            parser.decode(req, false).value.flatMap(_.fold(e =>
//              e.toHttpResponse(req.httpVersion),
//              { body =>
//                 `asInstanceOf` to turn the untyped HList to type T
//                route.action.act(req, (body :: i).asInstanceOf[T])
//              }))
          }
        }
    }
  }

  //////////////////////////////////////////////////////

  object Leaf {
    def apply[F[_]](f: (Request[F], HList) => Action[F]): Leaf[F] = SingleLeaf(f)
  }

  /** Leaves of the PathTree */
  sealed trait Leaf[F[_]] {
    /** Attempt to match this leaf */
    def attempt(req: Request[F], stack: HList): Action[F]

    /** Concatenate this leaf with another, giving the first precedence */
    final def ++(l: Leaf[F]): Leaf[F] = (this, l) match {
      case (s1@SingleLeaf(_), s2@SingleLeaf(_)) => ListLeaf(s1 :: s2 :: Nil)
      case (s1@SingleLeaf(_), ListLeaf(l)) => ListLeaf(s1 :: l)
      case (ListLeaf(l), s2@SingleLeaf(_)) => ListLeaf(l :+ s2)
      case (ListLeaf(l1), ListLeaf(l2)) => ListLeaf(l1 ::: l2)
    }
  }

  final private case class SingleLeaf[F[_]](f: (Request[F], HList) => Action[F]) extends Leaf[F] {
    override def attempt(req: Request[F], stack: HList): Action[F] = {
      try f(req, stack)
      catch { case NonFatal(t) =>
        logger.error(t)("Error in action execution")
        SuccessResponse(Response(status = Status.InternalServerError))
      }
    }
  }


  final private case class ListLeaf[F[_]](leaves: List[SingleLeaf[F]]) extends Leaf[F] {
    override def attempt(req: Request[F], stack: HList): Action[F] = {
      def go(l: List[SingleLeaf[F]], error: ResultResponse[F, Nothing]): Action[F] = {
        if (l.nonEmpty) l.head.attempt(req, stack) match {
          case r@SuccessResponse(_)     => r
          case e@FailureResponse(_)   => go(l.tail, if (error != null) error else e)
        }
        else if (error != null) error else sys.error("Leaf was empty!")
      }
      go(leaves, null)
    }
  }

  sealed trait Node[F[_], Self <: Node[F, Self]] {

    def matches: Map[String, MatchNode[F]]

    def captures: List[CaptureNode[F]]

    def variadic: Map[Method, Leaf[F]]

    def end: Map[Method, Leaf[F]]

    def clone(matches: Map[String, MatchNode[F]], captures: List[CaptureNode[F]], variadic: Map[Method, Leaf[F]], end: Map[Method, Leaf[F]]): Self

    def merge(other: Self): Self

    final def append(tail: PathRule, method: Method, action: Leaf[F]): Self = append(tail::Nil, method, action)

    // Add elements to the tree
    final protected def append(tail: List[PathRule], method: Method, action: Leaf[F]): Self = tail match {
      case h::t => h match {
        case PathAnd(p1, p2) => append(p1::p2::t, method, action)

        case PathOr(p1, p2) => append(p1::t, method, action).append(p2::t, method, action)

        case MetaCons(r, _) => append(r::t, method, action)  // discard metadata

        case PathMatch("") if !t.isEmpty => append(t, method, action) // "" is a NOOP in the middle of a path

        // the rest of the types need to rewrite a node
        case PathMatch(s) =>
          val next = matches.getOrElse(s, MatchNode[F](s)).append(t, method, action)
          clone(matches.updated(s, next), captures, variadic, end)

        case PathCapture(_, _, p, _) =>
          val exists = captures.exists{ case CaptureNode(p1,_,_,_,_) => p1 eq p }
          val all = if (exists) captures.map {
            case n @ CaptureNode(p1,_,_,_,_) if p1 eq p => n.append(t, method, action)
            case n => n
          }
          else CaptureNode[F](p.asInstanceOf[StringParser[F, _]]).append(t, method, action)::captures

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
    final def walkTree(method: Method, req: Request[F])(implicit F: Monad[F]): RouteResult[F, Response[F]] = {
      val path = keyToPath(req)
      walk(method, req, path, HNil)
    }

    // This should scan all forward paths.
    final protected def walk(method: Method, req: Request[F], path: List[String], stack: HList)(implicit F: Monad[F]): RouteResult[F, Response[F]] = {
      def tryVariadic(result: RouteResult[F, Response[F]]): RouteResult[F , Response[F]] =
        variadic.get(method) match {
          case None    => result
          case Some(l) => l.attempt(req, path::stack)
        }

      path match {
        case h::t =>
          val exact: RouteResult[F, Response[F]] = matches.get(h) match {
            case Some(n) => n.walk(method, req, t, stack)
            case None    => NoMatch()
          }

          @tailrec
          def go(children: List[CaptureNode[F]], error: RouteResult[F, Response[F]]): RouteResult[F, Response[F]] = children match {
              case (c@CaptureNode(p,_,cs,_,_))::ns =>
                p.parse(h) match {
                  case SuccessResponse(r) =>
                    val n = c.walk(method, req, t, r::stack)
                    if (n.isSuccess)        n
                    else if (error.isEmpty) go(ns, n)
                    else                    go(ns, error)

                  case r @ FailureResponse(_) => go(ns, if (error.isEmpty) r.asInstanceOf[FailureResponse[F]] else error)
                }

              case Nil => tryVariadic(error)
          }

          if (exact.isSuccess) exact
          else go(captures, exact)

        case Nil =>
          end.get(method) match {
            case Some(l) => l.attempt(req, stack)
            case None =>
              val result = tryVariadic(NoMatch())

              if (!result.isEmpty || end.isEmpty || method == Method.OPTIONS) result
              else FailureResponse.pure {
                val ms = end.keys
                val allowedMethods = ms.mkString(", ")
                val msg = s"$method not allowed. Defined methods: $allowedMethods\n"

                F.map(MethodNotAllowed[F].pure(msg))(
                  _.putHeaders(headers.Allow(ms.head, ms.tail.toList:_*)))

                // TODO: remove this!
                ???
              }

          }
      }
    }
  }

  final case class MatchNode[F[_]](name:     String,
                                   matches:  Map[String, MatchNode[F]] = Map.empty[String, MatchNode[F]],
                                   captures: List[CaptureNode[F]] = Nil,
                                   variadic: Map[Method, Leaf[F]] = Map.empty[Method, Leaf[F]],
                                   end:      Map[Method, Leaf[F]] = Map.empty[Method, Leaf[F]]) extends Node[F, MatchNode[F]] {

    override def clone(matches: Map[String, MatchNode[F]], captures: List[CaptureNode[F]], variadic: Map[Method, Leaf[F]], end: Map[Method, Leaf[F]]): MatchNode[F] =
      copy(matches = matches, captures = captures, variadic = variadic, end = end)

    override def merge(other: MatchNode[F]): MatchNode[F] = {
      require(other.name == name, s"${this.getClass.getSimpleName}s don't have the same name, this: $name, that: ${other.name}.")

      clone(mergeMatches(matches, other.matches),
            mergeCaptures(captures, other.captures),
            mergeLeaves(variadic, other.variadic),
            mergeLeaves(end, other.end))
    }
  }

  final case class CaptureNode[F[_]](parser:   StringParser[F, _],
                                     matches:  Map[String, MatchNode[F]] = Map.empty[String, MatchNode[F]],
                                     captures: List[CaptureNode[F]] = Nil,
                                     variadic: Map[Method, Leaf[F]] = Map.empty[Method, Leaf[F]],
                                     end:      Map[Method, Leaf[F]] = Map.empty[Method, Leaf[F]]) extends Node[F, CaptureNode[F]] {

    override def clone(matches: Map[String, MatchNode[F]], captures: List[CaptureNode[F]], variadic: Map[Method, Leaf[F]], end: Map[Method, Leaf[F]]): CaptureNode[F] =
      copy(matches = matches, captures = captures, variadic = variadic, end = end)

    override def merge(other: CaptureNode[F]): CaptureNode[F] = {
      require(other.parser eq parser, s"Cannot merge ${this.getClass.getSimpleName}s that have different parsers")

      clone(mergeMatches(matches, other.matches),
            mergeCaptures(captures, other.captures),
            mergeLeaves(variadic, other.variadic),
            mergeLeaves(end, other.end))
    }
  }

  // This ordering can get a little funky here. It is defined such that nodes in c2 get promoted in order if
  // they are also found in c1.
  // TODO: should we just concat the lists and be done with it? That would make captures a linear search...
  private def mergeCaptures[F[_]](c1: List[CaptureNode[F]], c2: List[CaptureNode[F]]): List[CaptureNode[F]] = {
    val first = c1.map { c => c2.find(_.parser eq c.parser).map(c merge _).getOrElse(c) }
    val last  = c2.filterNot { c => c1.exists(_.parser eq c.parser) }
    first ++ last
  }

  private def mergeMatches[F[_]](m1: Map[String, MatchNode[F]], m2: Map[String, MatchNode[F]]): Map[String, MatchNode[F]] =
    mergeMaps(m1, m2)(_ merge _)

  private def mergeLeaves[F[_]](l1: Map[Method, Leaf[F]], l2: Map[Method, Leaf[F]]): Map[Method, Leaf [F]] =
    mergeMaps(l1, l2)(_ ++ _)

  private def mergeMaps[K,V](m1: Map[K,V], m2: Map[K,V])(merge: (V,V) => V): Map[K,V] = {
    val allMatches = m1.keySet ++ m2.keySet
    allMatches.toSeq.map{ k =>
      (m1.get(k), m2.get(k)) match {
        case (Some(l1), Some(l2)) => (k, merge(l1,l2))
        case (Some(l1), None)     => (k, l1)
        case (None, Some(l2))     => (k, l2)
        case (None, None)         => sys.error("Shouldn't get here: each name should be defined in at least one of the maps")
      }
    }.toMap
  }
}
