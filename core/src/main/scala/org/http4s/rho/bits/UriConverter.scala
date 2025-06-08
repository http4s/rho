/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.rho.bits

import org.http4s.UriTemplate.Path
import org.http4s.UriTemplate.Query

import PathAST.PathRule
import org.http4s.rho.bits.RequestAST._

import scala.util.{Failure, Success, Try}

/** Helps to convert different parts of a route into an `UriTemplate` */
object UriConverter {

  def createPath(rule: PathRule): Try[Path] = {
    import PathAST._
    import org.http4s.UriTemplate.PathElm
    import org.http4s.UriTemplate.PathExp
    @scala.annotation.tailrec
    def go(r: List[PathRule], acc: Path): Path = r match {
      case Nil => acc
      case PathAnd(a, b) :: rs => go(a :: b :: rs, acc)
      case PathOr(a, _) :: rs => go(a :: rs, acc) // we decided to take the first root
      case PathMatch.empty :: rs => go(rs, acc)
      case PathMatch(s) :: rs => go(rs, PathElm(s.encoded) :: acc)
      case PathCapture(id, _, _, _) :: rs => go(rs, PathExp(id) :: acc)
      case CaptureTail :: rs => go(rs, acc)
      case MetaCons(p, _) :: rs => go(p :: rs, acc)
    }
    Success(go(List(rule), Nil).reverse)
  }

  def createQuery[F[_]](rule: RequestRule[F]): Try[Query] = {
    import org.http4s.UriTemplate.ParamExp
    @scala.annotation.tailrec
    def go(rule: List[RequestRule[F]], acc: Query): Try[Query] = rule match {
      case Nil => Success(acc.reverse)
      case MetaRule(r, QueryMetaData(n, _, _, _, _)) :: rs => go(r :: rs, ParamExp(n) :: acc)
      case MetaRule(r, _) :: rs => go(r :: rs, acc)
      case AndRule(a, b) :: rs => go(a :: b :: rs, acc)
      case (EmptyRule() | CaptureRule(_)) :: rs => go(rs, acc)
      case MapRule(a, _) :: rs => go(a :: rs, acc)
      case IgnoreRule(r) :: rs => go(r :: rs, acc)
      case OrRule(_, _) :: _ => Failure(new Exception("Cannot create a query from 'or'ed paths"))
    }
    go(List(rule), Nil)
  }

}
