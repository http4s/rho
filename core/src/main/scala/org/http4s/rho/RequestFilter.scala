package org.http4s
package rho

import bits.{ResultMatcher, HListToFunc}
import shapeless.HList

import scalaz.concurrent.Task

/**
 * Created on 7/9/15.
 */

final class RequestFilter[R: ResultMatcher] private(filter: Request => Task[Option[R]]) {
  def ^?>[H <: HList, F](f: F)(implicit ev: HListToFunc[H, F]): RequestFiltered[H, F, R] = RequestFiltered(f, filter)
  def filtering[H <: HList, F](f: F)(implicit ev: HListToFunc[H, F]): RequestFiltered[H, F, R] = ^?>(f)
}

object RequestFilter {
  def filter[R: ResultMatcher](filter: Request => Task[Option[R]]): RequestFilter[R] = new RequestFilter(filter)
  def pureFilter[R: ResultMatcher](f: Request => Option[R]): RequestFilter[R] = filter(f.andThen(Task.now))
}

final case class RequestFiltered[H <: HList, +F, R](f: F, filter: Request => Task[Option[R]])(implicit ev: HListToFunc[H, F], ev2: ResultMatcher[R])

object RequestFiltered {
  implicit def beforeFiltered[H <: HList, F, R](implicit ev: HListToFunc[H, F], rev: ResultMatcher[R]): HListToFunc[H, RequestFiltered[H, F, R]] = new HListToFunc[H, RequestFiltered[H, F, R]] {
    override def matcher = ev.matcher.addInfo(rev.encodings, rev.resultInfo)

    override def conv(beforeFilter: RequestFiltered[H, F, R]): (Request, H) => Task[Response] = {
      // cache the inner converter in case it does a lot of 'stuff'
      val conv = ev.conv(beforeFilter.f)
      (req, h) => for {
                    filtered <- beforeFilter.filter(req)
                    r <- filtered match {
                           case Some(r) => rev.conv(req, r)
                           case None    => conv(req, h)
                         }
                  } yield r
    }
  }
}
