package org.http4s
package rho
package bits

import shapeless.HList
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scalaz.concurrent.Task

/** Rho specific implementation of the PathTree */
final class RhoPathTree extends PathTree {
  import RhoPathTree._

  private var paths: Node = HeadNode()

  override type Key = Request

  override type Value = () => Task[Response]

  override def toString = paths.toString()

  /** Generates a list of tokens that represent the path */
  override def keyToPath(key: Key): List[String] = splitPath(key.pathInfo)

  def appendAction[T <: HList, F](action: RhoAction[T, F]): Unit = {
    val m = action.method
    val newLeaf = makeLeaf(action)
    val newNode = paths.append(action.path, m, newLeaf)
    paths = newNode
  }

  def getResult(req: Request): RouteResult[Value] = paths.walkTree(req.method, req)

  private def makeLeaf[T <: HList, F, O](action: RhoAction[T, F]): Leaf = {
    action.router match {
      case Router(method, _, query, vals) =>
        Leaf(query, vals, None){ (req, pathstack) =>
          for {
            i <- ValidationTools.runQuery(req, query, pathstack)
            j <- ValidationTools.runValidation(req, vals, i) // `asInstanceOf` to turn the untyped HList to type T
          } yield () => action.hf.conv(action.f)(req, j.asInstanceOf[T])
        }

      case c @ CodecRouter(_, parser) =>
        val actionf = action.hf.conv(action.f)
        Leaf(c.router.query, c.headers, Some(parser)){ (req, pathstack) =>
          for {
            i <- ValidationTools.runQuery(req, c.router.query, pathstack)
            j <- ValidationTools.runValidation(req, c.headers, i)
          } yield () => parser.decode(req).run.flatMap(_.fold(e =>
            Response(Status.BadRequest, req.httpVersion).withBody(e.sanitized),
          { body =>
            // `asInstanceOf` to turn the untyped HList to type T
            actionf(req, (body :: j).asInstanceOf[T])
          }))
        }
    }
  }

}

private[rho] object RhoPathTree {

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

}
