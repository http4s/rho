package org.http4s
package rho
package bits

import shapeless.HList
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalaz.concurrent.Task

/** Rho specific implementation of the PathTree */
final class RhoPathTree extends PathTree {
  import RhoPathTree._

  private val methods: mutable.Map[Method, Node] = mutable.HashMap.empty

  override type Key = Request

  override type Value = () => Task[Response]

  override def toString() = methods.toString()

  /** Generates a list of tokens that represent the path */
  override def keyToPath(key: Key): List[String] = splitPath(key.uri.path)

  def appendAction[T <: HList, F](action: RhoAction[T, F]): Unit = {
    val m = action.method
    val newLeaf = makeLeaf(action)
    val newNode = methods.get(m).getOrElse(HeadNode()).append(action.path, newLeaf)
    methods(m) = newNode
  }

  def getResult(req: Request): RouteResult[Value] = {
    methods.get(req.method) match {
      case Some(method) => method.walkTree(req)
      case None => NoMatch
    }
  }

  private def makeLeaf[T <: HList, F, O](action: RhoAction[T, F]): Leaf = {
    action.router match {
      case Router(method, _, query, vals) =>
        SingleLeaf(query, vals, None, (req, pathstack) => {
          for {
            i <- TempTools.runQuery(req, query, pathstack)
            j <- TempTools.runValidation(req, vals, i) // `asInstanceOf` to turn the untyped HList to type T
          } yield (() => action.hf.conv(action.f)(req, j.asInstanceOf[T]).map(_.resp))
        })

      case c @ CodecRouter(_, parser) =>
        val actionf = action.hf.conv(action.f)
        SingleLeaf(c.router.query, c.headers, Some(parser), {
          (req, pathstack) =>
            for {
              i <- TempTools.runQuery(req, c.router.query, pathstack)
              j <- TempTools.runValidation(req, c.headers, i)
            } yield (() => {
              parser.decode(req).flatMap { // `asInstanceOf` to turn the untyped HList to type T
                case ParserSuccess(r)     => actionf(req, (r :: pathstack).asInstanceOf[T]).map(_.resp)
                case ParserFailure(e)     => TempTools.onBadRequest(s"Decoding error: $e")
                case ValidationFailure(e) => TempTools.onBadRequest(s"Validation failure: $e")
              }
            })
        })
    }
  }

}

private object RhoPathTree {

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
