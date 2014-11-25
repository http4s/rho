package org.http4s
package rho.bits

import org.http4s.rho.Result
import shapeless.{ HList, HNil, :: }
import scalaz.concurrent.Task

/////////////////// Helpers for turning a function of may params to a function of a HList
// The library https://github.com/sbt/sbt-boilerplate may be useful for final generation

/**
 * Maps an F to the HList T
 * @tparam T HList type of the incoming values
 * @tparam F type of element onto which T will be mapped
 */
trait HListToFunc[T <: HList, -F] {
  def matcher: ResultMatcher[_]
  def conv(f: F): (Request, T) => Task[Response]
  def encodings: Set[MediaType] = matcher.encodings
  def resultInfo: Set[ResultInfo] = matcher.resultInfo
}

object HListToFunc {

  implicit def const0[R](implicit m: ResultMatcher[R]) = new HListToFunc[HNil,R] {
    override def matcher = m
    override def conv(r: R): (Request, HNil) => Task[Response] = (req, _) => m.conv(req, r)
  }

  implicit def wReqFun0[S, R](implicit m: ResultMatcher[R]) = new HListToFunc[HNil, (Request) => R] {
    override def matcher: ResultMatcher[_] = m
    override def conv(f: (Request) => R): (Request, HNil) => Task[Response] = (req, _) =>
      m.conv(req, f(req))
  }

  implicit def fun0[R](implicit m: ResultMatcher[R]) = new HListToFunc[HNil, () => R] {
    override def matcher = m
    override def conv(f: () => R): (Request, HNil) => Task[Response] = (req, _) => m.conv(req, f())
  }

  implicit def fun1[T1, R](implicit m: ResultMatcher[R]) = new HListToFunc[T1 :: HNil, T1 => R] {
    override def matcher = m
    override def conv(f: (T1) => R): (Request, T1 :: HNil) => Task[Response] =
      (req, h) => m.conv(req, f(h.head))
  }

  implicit def wReqfun1[T1, R](implicit m: ResultMatcher[R]) = new HListToFunc[T1 :: HNil, (Request, T1) => R] {
    override def matcher = m
    override def conv(f: (Request, T1) => R): (Request, T1 :: HNil) => Task[Response] =
      (req, h) => m.conv(req, f(req, h.head))
  }

  implicit def fun2[T1, T2, R](implicit m: ResultMatcher[R]) = new HListToFunc[T1 :: T2 :: HNil, (T2, T1) => R] {
    override def matcher = m
    override def conv(f: (T2, T1) => R): (Request, T1 :: T2 :: HNil) => Task[Response] = { (req, h) =>
      m.conv(req, f(h.tail.head, h.head))
    }
  }

  implicit def wReqfun2[T1, T2, R](implicit m: ResultMatcher[R]) = new HListToFunc[T1 :: T2 :: HNil, (Request, T2, T1) => R] {
    override def matcher = m
    override def conv(f: (Request, T2, T1) => R): (Request, T1 :: T2 :: HNil) => Task[Response] = { (req, h) =>
      m.conv(req, f(req, h.tail.head, h.head))
    }
  }

  implicit def fun3[T1, T2, T3, R](implicit m: ResultMatcher[R]) = new HListToFunc[T3 :: T2 :: T1 :: HNil, (T1, T2, T3) => R] {
    override def matcher = m
    override def conv(f: (T1, T2, T3) => R): (Request, T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h3) =>
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(t1, t2, t3))
    }
  }

  implicit def wReqfun3[T1, T2, T3, R](implicit m: ResultMatcher[R]) = new HListToFunc[T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3) => R] {
    override def matcher = m
    override def conv(f: (Request, T1, T2, T3) => R): (Request, T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h3) =>
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(req, t1, t2, t3))
    }
  }

  implicit def fun4[T1, T2, T3, T4, R](implicit m: ResultMatcher[R]) = new HListToFunc[T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4) => R] {
    override def matcher = m
    override def conv(f: (T1, T2, T3, T4) => R): (Request, T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h4) =>
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(t1, t2, t3, t4))
    }
  }

  implicit def wReqfun4[T1, T2, T3, T4, R](implicit m: ResultMatcher[R]) = new HListToFunc[T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4) => R] {
    override def matcher = m
    override def conv(f: (Request, T1, T2, T3, T4) => R): (Request, T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h4) =>
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(req, t1, t2, t3, t4))
    }
  }

  implicit def fun5[T1, T2, T3, T4, T5, R](implicit m: ResultMatcher[R]) = new HListToFunc[T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5) => R] {
    override def matcher = m
    override def conv(f: (T1, T2, T3, T4, T5) => R): (Request, T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h5) =>
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(t1, t2, t3, t4, t5))
    }
  }

  implicit def wReqfun5[T1, T2, T3, T4, T5, R](implicit m: ResultMatcher[R]) = new HListToFunc[T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5) => R] {
    override def matcher = m
    override def conv(f: (Request, T1, T2, T3, T4, T5) => R): (Request, T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h5) =>
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(req, t1, t2, t3, t4, t5))
    }
  }

  implicit def fun6[T1, T2, T3, T4, T5, T6, R](implicit m: ResultMatcher[R]) = new HListToFunc[T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5, T6) => R] {
    override def matcher = m
    override def conv(f: (T1, T2, T3, T4, T5, T6) => R): (Request, T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h6) =>
      val t6 = h6.head
      val h5 = h6.tail
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(t1, t2, t3, t4, t5, t6))
    }
  }

  implicit def wReqfun6[T1, T2, T3, T4, T5, T6, R](implicit m: ResultMatcher[R]) = new HListToFunc[T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5, T6) => R] {
    override def matcher = m
    override def conv(f: (Request, T1, T2, T3, T4, T5, T6) => R): (Request, T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h6) =>
      val t6 = h6.head
      val h5 = h6.tail
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(req, t1, t2, t3, t4, t5, t6))
    }
  }

  implicit def fun7[T1, T2, T3, T4, T5, T6, T7, R](implicit m: ResultMatcher[R]) = new HListToFunc[T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5, T6, T7) => R] {
    override def matcher = m
    override def conv(f: (T1, T2, T3, T4, T5, T6, T7) => R): (Request, T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h7) =>
      val t7 = h7.head
      val h6 = h7.tail
      val t6 = h6.head
      val h5 = h6.tail
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(t1, t2, t3, t4, t5, t6, t7))
    }
  }

  implicit def wReqfun7[T1, T2, T3, T4, T5, T6, T7, R](implicit m: ResultMatcher[R]) = new HListToFunc[T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5, T6, T7) => R] {
    override def matcher = m
    override def conv(f: (Request, T1, T2, T3, T4, T5, T6, T7) => R): (Request, T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h7) =>
      val t7 = h7.head
      val h6 = h7.tail
      val t6 = h6.head
      val h5 = h6.tail
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(req, t1, t2, t3, t4, t5, t6, t7))
    }
  }

  implicit def fun8[T1, T2, T3, T4, T5, T6, T7, T8, R](implicit m: ResultMatcher[R]) = new HListToFunc[T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5, T6, T7, T8) => R] {
    override def matcher = m
    override def conv(f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): (Request, T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h8) =>
      val t8 = h8.head
      val h7 = h8.tail
      val t7 = h7.head
      val h6 = h7.tail
      val t6 = h6.head
      val h5 = h6.tail
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(t1, t2, t3, t4, t5, t6, t7, t8))
    }
  }

  implicit def wReqfun8[T1, T2, T3, T4, T5, T6, T7, T8, R](implicit m: ResultMatcher[R]) = new HListToFunc[T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5, T6, T7, T8) => R] {
    override def matcher = m
    override def conv(f: (Request, T1, T2, T3, T4, T5, T6, T7, T8) => R): (Request, T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h8) =>
      val t8 = h8.head
      val h7 = h8.tail
      val t7 = h7.head
      val h6 = h7.tail
      val t6 = h6.head
      val h5 = h6.tail
      val t5 = h5.head
      val h4 = h5.tail
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      m.conv(req, f(req, t1, t2, t3, t4, t5, t6, t7, t8))
    }
  }
}

