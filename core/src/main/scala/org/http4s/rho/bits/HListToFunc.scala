package org.http4s
package rho.bits

import org.http4s.rho.Result
import shapeless.{ HList, HNil, :: }
import scalaz.concurrent.Task

import scala.reflect.runtime.universe.TypeTag

/////////////////// Helpers for turning a function of may params to a function of a HList
// The library https://github.com/sbt/sbt-boilerplate may be useful for final generation

/**
 * Maps an F to the HList T
 * @tparam T HList type of the incoming values
 * @tparam F type of element onto which T will be mapped
 */
trait HListToFunc[T <: HList, -F] {
  def conv(f: F): (Request, T) => Task[Response]
  def encodings: Set[MediaType]
  def typeTag: TypeTag[_]
}

object HListToFunc {

  implicit def wReqFun0[O](implicit t: TypeTag[O]) = new HListToFunc[HNil, (Request) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (Request) => Task[Result[O]]): (Request, HNil) => Task[Response] = (req, _) =>
      f(req).map(_.resp)
  }

  implicit def fun0[O](implicit t: TypeTag[O]) = new HListToFunc[HNil, () => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: () => Task[Result[O]]): (Request, HNil) => Task[Response] = (_, _) => f().map(_.resp)
  }

  implicit def fun1[T1, O](implicit t: TypeTag[O]) = new HListToFunc[T1 :: HNil, T1 => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (T1) => Task[Result[O]]): (Request, T1 :: HNil) => Task[Response] =
      (_, h) => f(h.head).map(_.resp)
  }

  implicit def wReqfun1[T1, O](implicit t: TypeTag[O]) = new HListToFunc[T1 :: HNil, (Request, T1) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (Request, T1) => Task[Result[O]]): (Request, T1 :: HNil) => Task[Response] =
      (req, h) => f(req, h.head).map(_.resp)
  }

  implicit def fun2[T1, T2, O](implicit t: TypeTag[O]) = new HListToFunc[T1 :: T2 :: HNil, (T2, T1) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (T2, T1) => Task[Result[O]]): (Request, T1 :: T2 :: HNil) => Task[Response] = { (_, h) =>
      f(h.tail.head, h.head).map(_.resp) }
  }

  implicit def wReqfun2[T1, T2, O](implicit t: TypeTag[O]) = new HListToFunc[T1 :: T2 :: HNil, (Request, T2, T1) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (Request, T2, T1) => Task[Result[O]]): (Request, T1 :: T2 :: HNil) => Task[Response] = { (req, h) =>
      f(req, h.tail.head, h.head).map(_.resp)
    }
  }

  implicit def fun3[T1, T2, T3, O](implicit t: TypeTag[O]) = new HListToFunc[T3 :: T2 :: T1 :: HNil, (T1, T2, T3) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (T1, T2, T3) => Task[Result[O]]): (Request, T3 :: T2 :: T1 :: HNil) => Task[Response] = { (_, h3) =>
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      f(t1, t2, t3).map(_.resp)
    }
  }

  implicit def wReqfun3[T1, T2, T3, O](implicit t: TypeTag[O]) = new HListToFunc[T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (Request, T1, T2, T3) => Task[Result[O]]): (Request, T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h3) =>
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      f(req, t1, t2, t3).map(_.resp)
    }
  }

  implicit def fun4[T1, T2, T3, T4, O](implicit t: TypeTag[O]) = new HListToFunc[T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (T1, T2, T3, T4) => Task[Result[O]]): (Request, T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (_, h4) =>
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      f(t1, t2, t3, t4).map(_.resp)
    }
  }

  implicit def wReqfun4[T1, T2, T3, T4, O](implicit t: TypeTag[O]) = new HListToFunc[T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4) => Task[Result[O]]] {
    override def encodings: Set[MediaType] = ???
    override def typeTag: TypeTag[O] = t
    override def conv(f: (Request, T1, T2, T3, T4) => Task[Result[O]]): (Request, T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h4) =>
      val t4 = h4.head
      val h3 = h4.tail
      val t3 = h3.head
      val h2 = h3.tail
      val t2 = h2.head
      val t1 = h2.tail.head
      f(req, t1, t2, t3, t4).map(_.resp)
    }
  }
//
//  implicit def fun5[T1, T2, T3, T4, T5, O](implicit o: ObjToResponse[O]) = new HListToFunc[T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (T1, T2, T3, T4, T5) => O): (Request, T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (_, h5) =>
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(t1, t2, t3, t4, t5))
//    }
//  }
//
//  implicit def wReqfun5[T1, T2, T3, T4, T5, O](implicit o: ObjToResponse[O]) = new HListToFunc[T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (Request, T1, T2, T3, T4, T5) => O): (Request, T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h5) =>
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(req, t1, t2, t3, t4, t5))
//    }
//  }
//
//  implicit def fun6[T1, T2, T3, T4, T5, T6, O](implicit o: ObjToResponse[O]) = new HListToFunc[T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5, T6) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (T1, T2, T3, T4, T5, T6) => O): (Request, T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (_, h6) =>
//      val t6 = h6.head
//      val h5 = h6.tail
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(t1, t2, t3, t4, t5, t6))
//    }
//  }
//
//  implicit def wReqfun6[T1, T2, T3, T4, T5, T6, O](implicit o: ObjToResponse[O]) = new HListToFunc[T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5, T6) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (Request, T1, T2, T3, T4, T5, T6) => O): (Request, T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h6) =>
//      val t6 = h6.head
//      val h5 = h6.tail
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(req, t1, t2, t3, t4, t5, t6))
//    }
//  }
//
//  implicit def fun7[T1, T2, T3, T4, T5, T6, T7, O](implicit o: ObjToResponse[O]) = new HListToFunc[T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5, T6, T7) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (T1, T2, T3, T4, T5, T6, T7) => O): (Request, T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (_, h7) =>
//      val t7 = h7.head
//      val h6 = h7.tail
//      val t6 = h6.head
//      val h5 = h6.tail
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(t1, t2, t3, t4, t5, t6, t7))
//    }
//  }
//
//  implicit def wReqfun7[T1, T2, T3, T4, T5, T6, T7, O](implicit o: ObjToResponse[O]) = new HListToFunc[T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5, T6, T7) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (Request, T1, T2, T3, T4, T5, T6, T7) => O): (Request, T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h7) =>
//      val t7 = h7.head
//      val h6 = h7.tail
//      val t6 = h6.head
//      val h5 = h6.tail
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(req, t1, t2, t3, t4, t5, t6, t7))
//    }
//  }
//
//  implicit def fun8[T1, T2, T3, T4, T5, T6, T7, T8, O](implicit o: ObjToResponse[O]) = new HListToFunc[T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (T1, T2, T3, T4, T5, T6, T7, T8) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (T1, T2, T3, T4, T5, T6, T7, T8) => O): (Request, T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (_, h8) =>
//      val t8 = h8.head
//      val h7 = h8.tail
//      val t7 = h7.head
//      val h6 = h7.tail
//      val t6 = h6.head
//      val h5 = h6.tail
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(t1, t2, t3, t4, t5, t6, t7, t8))
//    }
//  }
//
//  implicit def wReqfun8[T1, T2, T3, T4, T5, T6, T7, T8, O](implicit o: ObjToResponse[O]) = new HListToFunc[T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil, (Request, T1, T2, T3, T4, T5, T6, T7, T8) => O] {
//    override def encodings: Set[MediaType] = o.mediaTypes
//    override def typeTag: Option[TypeTag[O]] = o.typeTag
//    override def conv(f: (Request, T1, T2, T3, T4, T5, T6, T7, T8) => O): (Request, T8 :: T7 :: T6 :: T5 :: T4 :: T3 :: T2 :: T1 :: HNil) => Task[Response] = { (req, h8) =>
//      val t8 = h8.head
//      val h7 = h8.tail
//      val t7 = h7.head
//      val h6 = h7.tail
//      val t6 = h6.head
//      val h5 = h6.tail
//      val t5 = h5.head
//      val h4 = h5.tail
//      val t4 = h4.head
//      val h3 = h4.tail
//      val t3 = h3.head
//      val h2 = h3.tail
//      val t2 = h2.head
//      val t1 = h2.tail.head
//      o(f(req, t1, t2, t3, t4, t5, t6, t7, t8))
//    }
//  }

}

