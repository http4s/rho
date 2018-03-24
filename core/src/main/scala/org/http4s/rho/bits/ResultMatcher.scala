package org.http4s.rho.bits

import cats.Monad
import org.http4s.rho.Result
import org.http4s.{EntityEncoder, MediaType, Request, Response, Status}

import scala.reflect.runtime.universe.{Type, WeakTypeTag}

trait ResultMatcher[F[_], -R] {
  def encodings: Set[MediaType]
  def resultInfo: Set[ResultInfo]

  def conv(req: Request[F], r: R)(implicit F: Monad[F]): F[Response[F]]
}

object ResultMatcher extends ResultMatcherOps {

  sealed trait MaybeWritable[T] {
    def contentType: Set[MediaType]
    def resultInfo: Option[Type]
    def encodings: Set[MediaType]
  }

  object MaybeWritable extends MaybeWritableOps {
    type Aux[F[_], T] = MaybeWritable[T]

    // This will represent a "missing" result meaning this status wasn't used
    implicit val maybeWritableAny: MaybeWritable[Any] = new MaybeWritable[Any] {
      override def contentType: Set[MediaType] = Set.empty
      override def encodings: Set[MediaType] = Set.empty
      override def resultInfo: Option[Type] = None
    }
  }

  trait MaybeWritableOps {
    /* Allowing the `Writable` to be `null` only matches real results but allows for
       situations where you return the same status with two types */
    implicit def maybeIsWritable[F[_], T](implicit t: WeakTypeTag[T], w: EntityEncoder[F, T] = null): MaybeWritable.Aux[F, T] = new MaybeWritable[T] {
      private val ww = Option(w)
      override def contentType: Set[MediaType] = ww.flatMap(_.contentType.map(_.mediaType)).toSet
      override def encodings: Set[MediaType] = ww.flatMap(_.contentType.map(_.mediaType)).toSet
      override def resultInfo: Option[Type] = Some(t.tpe.dealias)
    }
  }

  implicit def statusMatcher[
  F[_],
  CONTINUE,
  SWITCHINGPROTOCOLS,
  PROCESSING,

  OK,
  CREATED,
  ACCEPTED,
  NONAUTHORITATIVEINFORMATION,
  NOCONTENT,
  RESETCONTENT,
  PARTIALCONTENT,
  MULTISTATUS,
  ALREADYREPORTED,
  IMUSED,

  MULTIPLECHOICES,
  MOVEDPERMANENTLY,
  FOUND,
  SEEOTHER,
  NOTMODIFIED,
  USEPROXY,
  TEMPORARYREDIRECT,
  PERMANENTREDIRECT,

  BADREQUEST,
  UNAUTHORIZED,
  PAYMENTREQUIRED,
  FORBIDDEN,
  NOTFOUND,
  METHODNOTALLOWED,
  NOTACCEPTABLE,
  PROXYAUTHENTICATIONREQUIRED,
  REQUESTTIMEOUT,
  CONFLICT,
  GONE,
  LENGTHREQUIRED,
  PRECONDITIONFAILED,
  PAYLOADTOOLARGE,
  URITOOLONG,
  UNSUPPORTEDMEDIATYPE,
  RANGENOTSATISFIABLE,
  EXPECTATIONFAILED,
  UNPROCESSABLEENTITY,
  LOCKED,
  FAILEDDEPENDENCY,
  UPGRADEREQUIRED,
  PRECONDITIONREQUIRED,
  TOOMANYREQUESTS,
  REQUESTHEADERFIELDSTOOLARGE,

  INTERNALSERVERERROR,
  NOTIMPLEMENTED,
  BADGATEWAY,
  SERVICEUNAVAILABLE,
  GATEWAYTIMEOUT,
  HTTPVERSIONNOTSUPPORTED,
  VARIANTALSONEGOTIATES,
  INSUFFICIENTSTORAGE,
  LOOPDETECTED,
  NOTEXTENDED,
  NETWORKAUTHENTICATIONREQUIRED](implicit mCONTINUE: MaybeWritable.Aux[F, CONTINUE],
                                mSWITCHINGPROTOCOLS: MaybeWritable.Aux[F, SWITCHINGPROTOCOLS],
                                        mPROCESSING: MaybeWritable.Aux[F, PROCESSING],
                                                mOK: MaybeWritable.Aux[F, OK],
                                           mCREATED: MaybeWritable.Aux[F, CREATED],
                                          mACCEPTED: MaybeWritable.Aux[F, ACCEPTED],
                       mNONAUTHORITATIVEINFORMATION: MaybeWritable.Aux[F, NONAUTHORITATIVEINFORMATION],
                                         mNOCONTENT: MaybeWritable.Aux[F, NOCONTENT],
                                      mRESETCONTENT: MaybeWritable.Aux[F, RESETCONTENT],
                                    mPARTIALCONTENT: MaybeWritable.Aux[F, PARTIALCONTENT],
                                       mMULTISTATUS: MaybeWritable.Aux[F, MULTISTATUS],
                                   mALREADYREPORTED: MaybeWritable.Aux[F, ALREADYREPORTED],
                                            mIMUSED: MaybeWritable.Aux[F, IMUSED],
                                   mMULTIPLECHOICES: MaybeWritable.Aux[F, MULTIPLECHOICES],
                                  mMOVEDPERMANENTLY: MaybeWritable.Aux[F, MOVEDPERMANENTLY],
                                             mFOUND: MaybeWritable.Aux[F, FOUND],
                                          mSEEOTHER: MaybeWritable.Aux[F, SEEOTHER],
                                       mNOTMODIFIED: MaybeWritable.Aux[F, NOTMODIFIED],
                                          mUSEPROXY: MaybeWritable.Aux[F, USEPROXY],
                                 mTEMPORARYREDIRECT: MaybeWritable.Aux[F, TEMPORARYREDIRECT],
                                 mPERMANENTREDIRECT: MaybeWritable.Aux[F, PERMANENTREDIRECT],
                                        mBADREQUEST: MaybeWritable.Aux[F, BADREQUEST],
                                      mUNAUTHORIZED: MaybeWritable.Aux[F, UNAUTHORIZED],
                                   mPAYMENTREQUIRED: MaybeWritable.Aux[F, PAYMENTREQUIRED],
                                         mFORBIDDEN: MaybeWritable.Aux[F, FORBIDDEN],
                                          mNOTFOUND: MaybeWritable.Aux[F, NOTFOUND],
                                  mMETHODNOTALLOWED: MaybeWritable.Aux[F, METHODNOTALLOWED],
                                     mNOTACCEPTABLE: MaybeWritable.Aux[F, NOTACCEPTABLE],
                       mPROXYAUTHENTICATIONREQUIRED: MaybeWritable.Aux[F, PROXYAUTHENTICATIONREQUIRED],
                                    mREQUESTTIMEOUT: MaybeWritable.Aux[F, REQUESTTIMEOUT],
                                          mCONFLICT: MaybeWritable.Aux[F, CONFLICT],
                                              mGONE: MaybeWritable.Aux[F, GONE],
                                    mLENGTHREQUIRED: MaybeWritable.Aux[F, LENGTHREQUIRED],
                                mPRECONDITIONFAILED: MaybeWritable.Aux[F, PRECONDITIONFAILED],
                                   mPAYLOADTOOLARGE: MaybeWritable.Aux[F, PAYLOADTOOLARGE],
                                        mURITOOLONG: MaybeWritable.Aux[F, URITOOLONG],
                              mUNSUPPORTEDMEDIATYPE: MaybeWritable.Aux[F, UNSUPPORTEDMEDIATYPE],
                               mRANGENOTSATISFIABLE: MaybeWritable.Aux[F, RANGENOTSATISFIABLE],
                                 mEXPECTATIONFAILED: MaybeWritable.Aux[F, EXPECTATIONFAILED],
                               mUNPROCESSABLEENTITY: MaybeWritable.Aux[F, UNPROCESSABLEENTITY],
                                            mLOCKED: MaybeWritable.Aux[F, LOCKED],
                                  mFAILEDDEPENDENCY: MaybeWritable.Aux[F, FAILEDDEPENDENCY],
                                   mUPGRADEREQUIRED: MaybeWritable.Aux[F, UPGRADEREQUIRED],
                              mPRECONDITIONREQUIRED: MaybeWritable.Aux[F, PRECONDITIONREQUIRED],
                                   mTOOMANYREQUESTS: MaybeWritable.Aux[F, TOOMANYREQUESTS],
                       mREQUESTHEADERFIELDSTOOLARGE: MaybeWritable.Aux[F, REQUESTHEADERFIELDSTOOLARGE],
                               mINTERNALSERVERERROR: MaybeWritable.Aux[F, INTERNALSERVERERROR],
                                    mNOTIMPLEMENTED: MaybeWritable.Aux[F, NOTIMPLEMENTED],
                                        mBADGATEWAY: MaybeWritable.Aux[F, BADGATEWAY],
                                mSERVICEUNAVAILABLE: MaybeWritable.Aux[F, SERVICEUNAVAILABLE],
                                    mGATEWAYTIMEOUT: MaybeWritable.Aux[F, GATEWAYTIMEOUT],
                           mHTTPVERSIONNOTSUPPORTED: MaybeWritable.Aux[F, HTTPVERSIONNOTSUPPORTED],
                             mVARIANTALSONEGOTIATES: MaybeWritable.Aux[F, VARIANTALSONEGOTIATES],
                               mINSUFFICIENTSTORAGE: MaybeWritable.Aux[F, INSUFFICIENTSTORAGE],
                                      mLOOPDETECTED: MaybeWritable.Aux[F, LOOPDETECTED],
                                       mNOTEXTENDED: MaybeWritable.Aux[F, NOTEXTENDED],
                     mNETWORKAUTHENTICATIONREQUIRED: MaybeWritable.Aux[F, NETWORKAUTHENTICATIONREQUIRED]): ResultMatcher[F, Result[
    F,
    CONTINUE,
    SWITCHINGPROTOCOLS,
    PROCESSING,

    OK,
    CREATED,
    ACCEPTED,
    NONAUTHORITATIVEINFORMATION,
    NOCONTENT,
    RESETCONTENT,
    PARTIALCONTENT,
    MULTISTATUS,
    ALREADYREPORTED,
    IMUSED,

    MULTIPLECHOICES,
    MOVEDPERMANENTLY,
    FOUND,
    SEEOTHER,
    NOTMODIFIED,
    USEPROXY,
    TEMPORARYREDIRECT,
    PERMANENTREDIRECT,

    BADREQUEST,
    UNAUTHORIZED,
    PAYMENTREQUIRED,
    FORBIDDEN,
    NOTFOUND,
    METHODNOTALLOWED,
    NOTACCEPTABLE,
    PROXYAUTHENTICATIONREQUIRED,
    REQUESTTIMEOUT,
    CONFLICT,
    GONE,
    LENGTHREQUIRED,
    PRECONDITIONFAILED,
    PAYLOADTOOLARGE,
    URITOOLONG,
    UNSUPPORTEDMEDIATYPE,
    RANGENOTSATISFIABLE,
    EXPECTATIONFAILED,
    UNPROCESSABLEENTITY,
    LOCKED,
    FAILEDDEPENDENCY,
    UPGRADEREQUIRED,
    PRECONDITIONREQUIRED,
    TOOMANYREQUESTS,
    REQUESTHEADERFIELDSTOOLARGE,

    INTERNALSERVERERROR,
    NOTIMPLEMENTED,
    BADGATEWAY,
    SERVICEUNAVAILABLE,
    GATEWAYTIMEOUT,
    HTTPVERSIONNOTSUPPORTED,
    VARIANTALSONEGOTIATES,
    INSUFFICIENTSTORAGE,
    LOOPDETECTED,
    NOTEXTENDED,
    NETWORKAUTHENTICATIONREQUIRED]] =
  new ResultMatcher[F, Result[
    F,
    CONTINUE,
    SWITCHINGPROTOCOLS,
    PROCESSING,

    OK,
    CREATED,
    ACCEPTED,
    NONAUTHORITATIVEINFORMATION,
    NOCONTENT,
    RESETCONTENT,
    PARTIALCONTENT,
    MULTISTATUS,
    ALREADYREPORTED,
    IMUSED,

    MULTIPLECHOICES,
    MOVEDPERMANENTLY,
    FOUND,
    SEEOTHER,
    NOTMODIFIED,
    USEPROXY,
    TEMPORARYREDIRECT,
    PERMANENTREDIRECT,

    BADREQUEST,
    UNAUTHORIZED,
    PAYMENTREQUIRED,
    FORBIDDEN,
    NOTFOUND,
    METHODNOTALLOWED,
    NOTACCEPTABLE,
    PROXYAUTHENTICATIONREQUIRED,
    REQUESTTIMEOUT,
    CONFLICT,
    GONE,
    LENGTHREQUIRED,
    PRECONDITIONFAILED,
    PAYLOADTOOLARGE,
    URITOOLONG,
    UNSUPPORTEDMEDIATYPE,
    RANGENOTSATISFIABLE,
    EXPECTATIONFAILED,
    UNPROCESSABLEENTITY,
    LOCKED,
    FAILEDDEPENDENCY,
    UPGRADEREQUIRED,
    PRECONDITIONREQUIRED,
    TOOMANYREQUESTS,
    REQUESTHEADERFIELDSTOOLARGE,

    INTERNALSERVERERROR,
    NOTIMPLEMENTED,
    BADGATEWAY,
    SERVICEUNAVAILABLE,
    GATEWAYTIMEOUT,
    HTTPVERSIONNOTSUPPORTED,
    VARIANTALSONEGOTIATES,
    INSUFFICIENTSTORAGE,
    LOOPDETECTED,
    NOTEXTENDED,
    NETWORKAUTHENTICATIONREQUIRED]] {
    override lazy val encodings: Set[MediaType] =
      allTpes.flatMap { case (_, m) => m.encodings }.toSet

    override def conv(req: Request[F], r: Result[
      F,
      CONTINUE,
      SWITCHINGPROTOCOLS,
      PROCESSING,

      OK,
      CREATED,
      ACCEPTED,
      NONAUTHORITATIVEINFORMATION,
      NOCONTENT,
      RESETCONTENT,
      PARTIALCONTENT,
      MULTISTATUS,
      ALREADYREPORTED,
      IMUSED,

      MULTIPLECHOICES,
      MOVEDPERMANENTLY,
      FOUND,
      SEEOTHER,
      NOTMODIFIED,
      USEPROXY,
      TEMPORARYREDIRECT,
      PERMANENTREDIRECT,

      BADREQUEST,
      UNAUTHORIZED,
      PAYMENTREQUIRED,
      FORBIDDEN,
      NOTFOUND,
      METHODNOTALLOWED,
      NOTACCEPTABLE,
      PROXYAUTHENTICATIONREQUIRED,
      REQUESTTIMEOUT,
      CONFLICT,
      GONE,
      LENGTHREQUIRED,
      PRECONDITIONFAILED,
      PAYLOADTOOLARGE,
      URITOOLONG,
      UNSUPPORTEDMEDIATYPE,
      RANGENOTSATISFIABLE,
      EXPECTATIONFAILED,
      UNPROCESSABLEENTITY,
      LOCKED,
      FAILEDDEPENDENCY,
      UPGRADEREQUIRED,
      PRECONDITIONREQUIRED,
      TOOMANYREQUESTS,
      REQUESTHEADERFIELDSTOOLARGE,

      INTERNALSERVERERROR,
      NOTIMPLEMENTED,
      BADGATEWAY,
      SERVICEUNAVAILABLE,
      GATEWAYTIMEOUT,
      HTTPVERSIONNOTSUPPORTED,
      VARIANTALSONEGOTIATES,
      INSUFFICIENTSTORAGE,
      LOOPDETECTED,
      NOTEXTENDED,
      NETWORKAUTHENTICATIONREQUIRED])(implicit F: Monad[F]): F[Response[F]] = F.pure(r.resp)

    override def resultInfo: Set[ResultInfo] = {
      allTpes.flatMap { case (s, mw) =>
        mw.resultInfo.map( t => StatusAndType(s, t))
      }.toSet
    }

    private lazy val allTpes: List[(org.http4s.Status, MaybeWritable[_])] = {
      List(
        (org.http4s.Status.Continue, mCONTINUE),
        (org.http4s.Status.SwitchingProtocols, mSWITCHINGPROTOCOLS),
        (org.http4s.Status.Processing, mPROCESSING),
        (org.http4s.Status.Ok, mOK),
        (org.http4s.Status.Created, mCREATED),
        (org.http4s.Status.Accepted, mACCEPTED),
        (org.http4s.Status.NonAuthoritativeInformation, mNONAUTHORITATIVEINFORMATION),
        (org.http4s.Status.NoContent, mNOCONTENT),
        (org.http4s.Status.ResetContent, mRESETCONTENT),
        (org.http4s.Status.PartialContent, mPARTIALCONTENT),
        (org.http4s.Status.MultiStatus, mMULTISTATUS),
        (org.http4s.Status.AlreadyReported, mALREADYREPORTED),
        (org.http4s.Status.IMUsed, mIMUSED),
        (org.http4s.Status.MultipleChoices, mMULTIPLECHOICES),
        (org.http4s.Status.MovedPermanently, mMOVEDPERMANENTLY),
        (org.http4s.Status.Found, mFOUND),
        (org.http4s.Status.SeeOther, mSEEOTHER),
        (org.http4s.Status.NotModified, mNOTMODIFIED),
        (org.http4s.Status.UseProxy, mUSEPROXY),
        (org.http4s.Status.TemporaryRedirect, mTEMPORARYREDIRECT),
        (org.http4s.Status.PermanentRedirect, mPERMANENTREDIRECT),
        (org.http4s.Status.BadRequest, mBADREQUEST),
        (org.http4s.Status.Unauthorized, mUNAUTHORIZED),
        (org.http4s.Status.PaymentRequired, mPAYMENTREQUIRED),
        (org.http4s.Status.Forbidden, mFORBIDDEN),
        (org.http4s.Status.NotFound, mNOTFOUND),
        (org.http4s.Status.MethodNotAllowed, mMETHODNOTALLOWED),
        (org.http4s.Status.NotAcceptable, mNOTACCEPTABLE),
        (org.http4s.Status.ProxyAuthenticationRequired, mPROXYAUTHENTICATIONREQUIRED),
        (org.http4s.Status.RequestTimeout, mREQUESTTIMEOUT),
        (org.http4s.Status.Conflict, mCONFLICT),
        (org.http4s.Status.Gone, mGONE),
        (org.http4s.Status.LengthRequired, mLENGTHREQUIRED),
        (org.http4s.Status.PreconditionFailed, mPRECONDITIONFAILED),
        (org.http4s.Status.PayloadTooLarge, mPAYLOADTOOLARGE),
        (org.http4s.Status.UriTooLong, mURITOOLONG),
        (org.http4s.Status.UnsupportedMediaType, mUNSUPPORTEDMEDIATYPE),
        (org.http4s.Status.RangeNotSatisfiable, mRANGENOTSATISFIABLE),
        (org.http4s.Status.ExpectationFailed, mEXPECTATIONFAILED),
        (org.http4s.Status.UnprocessableEntity, mUNPROCESSABLEENTITY),
        (org.http4s.Status.Locked, mLOCKED),
        (org.http4s.Status.FailedDependency, mFAILEDDEPENDENCY),
        (org.http4s.Status.UpgradeRequired, mUPGRADEREQUIRED),
        (org.http4s.Status.PreconditionRequired, mPRECONDITIONREQUIRED),
        (org.http4s.Status.TooManyRequests, mTOOMANYREQUESTS),
        (org.http4s.Status.RequestHeaderFieldsTooLarge, mREQUESTHEADERFIELDSTOOLARGE),
        (org.http4s.Status.InternalServerError, mINTERNALSERVERERROR),
        (org.http4s.Status.NotImplemented, mNOTIMPLEMENTED),
        (org.http4s.Status.BadGateway, mBADGATEWAY),
        (org.http4s.Status.ServiceUnavailable, mSERVICEUNAVAILABLE),
        (org.http4s.Status.GatewayTimeout, mGATEWAYTIMEOUT),
        (org.http4s.Status.HttpVersionNotSupported, mHTTPVERSIONNOTSUPPORTED),
        (org.http4s.Status.VariantAlsoNegotiates, mVARIANTALSONEGOTIATES),
        (org.http4s.Status.InsufficientStorage, mINSUFFICIENTSTORAGE),
        (org.http4s.Status.LoopDetected, mLOOPDETECTED),
        (org.http4s.Status.NotExtended, mNOTEXTENDED),
        (org.http4s.Status.NetworkAuthenticationRequired, mNETWORKAUTHENTICATIONREQUIRED)
      )
    }
  }

  implicit def optionMatcher[F[_], R](implicit o: WeakTypeTag[R], w: EntityEncoder[F, R]): ResultMatcher[F, Option[R]] = new ResultMatcher[F, Option[R]] with ResponseGeneratorInstances[F] {
    override val encodings: Set[MediaType] = w.contentType.map(_.mediaType).toSet
    override val resultInfo: Set[ResultInfo] = Set(StatusAndType(Status.Ok, o.tpe.dealias),
                                                   StatusOnly(Status.NotFound))

    override def conv(req: Request[F], r: Option[R])(implicit F: Monad[F]): F[Response[F]] = r match {
      case Some(res) => Ok.pure(res)
      case None      => NotFound.pure(req.uri.path)
    }
  }

  implicit def writableMatcher[F[_], R](implicit o: WeakTypeTag[R], w: EntityEncoder[F, R]): ResultMatcher[F, R] = new ResultMatcher[F, R] with ResponseGeneratorInstances[F] {
    override def encodings: Set[MediaType] = w.contentType.map(_.mediaType).toSet
    override def resultInfo: Set[ResultInfo] = Set(StatusAndType(Status.Ok, o.tpe.dealias))

    override def conv(req: Request[F], r: R)(implicit F: Monad[F]): F[Response[F]] = Ok.pure(r)
  }

  implicit def responseMatcher[F[_]]: ResultMatcher[F, Response[F]] = new ResultMatcher[F, Response[F]] {
    override def encodings: Set[MediaType] = Set.empty
    override def resultInfo: Set[ResultInfo] = Set.empty

    override def conv(req: Request[F], r: Response[F])(implicit F: Monad[F]): F[Response[F]] = F.pure(r)
  }
}

trait ResultMatcherOps {
  implicit def fMatcher[F[_], R](implicit r: ResultMatcher[F, R]): ResultMatcher[F, F[R]] = new ResultMatcher[F, F[R]] {
    override def encodings: Set[MediaType] = r.encodings
    override def resultInfo: Set[ResultInfo] = r.resultInfo

    override def conv(req: Request[F], f: F[R])(implicit F: Monad[F]): F[Response[F]] = F.flatMap(f)(r.conv(req, _))
  }
}
