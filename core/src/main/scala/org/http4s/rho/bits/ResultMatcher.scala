package org.http4s.rho.bits

import org.http4s.rho.Result.ExResult
import org.http4s.{Request, Status, Writable, MediaType}
import org.http4s.rho.Result
import scodec.bits.ByteVector

import scala.reflect.runtime.universe.{ Type, TypeTag }
import scalaz.\/
import scalaz.concurrent.Task


trait ResultMatcher[-R] {
  def encodings: Set[MediaType]
  def resultInfo: Set[ResultInfo]
  def conv(req: Request, r: R): Task[Result.ExResult]
}

object ResultMatcher extends Level0Impls {

  sealed trait MaybeWritable[T] {
    def contentType: Set[MediaType]
    def resultInfo: Option[Type]
    def encodings: Set[MediaType]
  }

  object MaybeWritable {

    // This will represent a "missing" result meaning this status wasn't used
    implicit val maybeWritableAny: MaybeWritable[Any] = new MaybeWritable[Any] {
      override def contentType: Set[MediaType] = Set.empty
      override def encodings: Set[MediaType] = Set.empty
      override def resultInfo: Option[Type] = None
    }

    implicit def maybeIsWritable[T](implicit w: Writable[T], t: TypeTag[T]): MaybeWritable[T] = new MaybeWritable[T] {
      override def contentType: Set[MediaType] = w.contentType.toSet
      override def encodings: Set[MediaType] = w.contentType.toSet
      override def resultInfo: Option[Type] = Some(t.tpe.dealias)
    }
  }

  implicit def megaMatcher[OK, NOTFOUND, NOCONTENT](implicit mOK: MaybeWritable[OK],
                                                       mNOTFOUND: MaybeWritable[NOTFOUND],
                                                      mNOCONTENT: MaybeWritable[NOCONTENT]): ResultMatcher[Result[OK, NOTFOUND, NOCONTENT]] =
  new ResultMatcher[Result[OK, NOTFOUND, NOCONTENT]] {
    override def encodings: Set[MediaType] = mOK.encodings ++ mNOCONTENT.encodings ++ mNOTFOUND.encodings

    override def conv(req: Request, r: Result[OK, NOTFOUND, NOCONTENT]): Task[ExResult] = Task.now(r)

    override def resultInfo: Set[ResultInfo] = {
      allTpes.flatMap { case (s, mw) =>
        mw.resultInfo.map( t => StatusAndModel(s, t))
      }.toSet
    }

    private val allTpes: List[(Status, MaybeWritable[_])] = {
      import Status._
      List(
        (Ok, mOK),
        (NotFound, mNOTFOUND),
        (NoContent, mNOCONTENT)
      )
    }
  }

//  implicit def resultMatcher[T <: Result.BaseResult, O](implicit s: TypeTag[T], o: TypeTag[O], w: Writable[O]): ResultMatcher[Result[S, O]] =
//    new ResultMatcher[Result[S, O]] {
//      override val encodings: Set[MediaType] = w.contentType.toSet
//      override def conv(req: Request, r: Result[S, O]): Task[Result.ExResult] = Task.now(r)
//
//      override val resultInfo: Set[ResultInfo] = ResultInfo.getStatus(s.tpe.dealias) match {
//        case Some(status) => Set(StatusAndModel(status, o.tpe.dealias))
//        case None         => Set(ModelOnly(o.tpe.dealias))
//      }
//    }

//  implicit def optionMatcher[O](implicit o: TypeTag[O], w: Writable[O]) = new ResultMatcher[Option[O]] {
//    override val encodings: Set[MediaType] = w.contentType.toSet
//    override val resultInfo: Set[ResultInfo] = Set(StatusAndModel(Status.Ok, o.tpe.dealias),
//                                                   StatusOnly(Status.NotFound))
//    override def conv(req: Request, r: Option[O]): Task[Result.ExResult] = r match {
//      case Some(r) => ResponseGeneratorInstances.Ok(r)
//      case None    => ResponseGeneratorInstances.NotFound(req.uri.path)
//    }
//  }
//
//  implicit def disjunctionMatcher[O1, O2](implicit r1: ResultMatcher[O1], r2: ResultMatcher[O2]): ResultMatcher[O1\/O2] =
//  new ResultMatcher[O1\/O2] {
//    override val encodings: Set[MediaType] = r1.encodings ++ r2.encodings
//    override val resultInfo: Set[ResultInfo] = r1.resultInfo ++ r2.resultInfo
//    override def conv(req: Request, r: \/[O1, O2]): Task[Result.ExResult] = r.fold(r1.conv(req, _), r2.conv(req, _))
//  }
//
  implicit def writableMatcher[O](implicit o: TypeTag[O], w: Writable[O]) = new ResultMatcher[O] {
    override def encodings: Set[MediaType] = w.contentType.toSet
    override def resultInfo: Set[ResultInfo] = Set(StatusAndModel(Status.Ok, o.tpe.dealias))
    override def conv(req: Request, r: O): Task[Result[O, Nothing, Nothing]] = ResponseGeneratorInstances.Ok(r)
  }
}


trait Level0Impls {
  implicit def taskMatcher[R](implicit r: ResultMatcher[R]): ResultMatcher[Task[R]] = new ResultMatcher[Task[R]] {
    override def encodings: Set[MediaType] = r.encodings
    override def resultInfo: Set[ResultInfo] = r.resultInfo
    override def conv(req: Request, t: Task[R]): Task[Result.ExResult] = t.flatMap(r.conv(req, _))
  }
}