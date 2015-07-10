package org.http4s
package rho


import scalaz.concurrent.Task

/**
 * Created on 7/10/15.
 */
trait BeforeFilter {
  /** Look at the [[Request]] and _possibly_ result in a [[Response]] */
  def before(req: Request): Task[Option[Response]]
}

object BeforeFilter {
  /*
    // filter is part of the path
    GET / "foo" # myFilter / 'bar |>> { bar: String => ??? }

    // filter is after the path (end of the path really)
    GET / "foo" / 'bar # myFilter |>> { bar: String => ??? }
   */

}
