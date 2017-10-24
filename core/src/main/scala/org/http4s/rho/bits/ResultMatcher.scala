package org.http4s.rho.bits

import cats.{Applicative, FlatMap, Monad}
import org.http4s.{EntityEncoder, MediaType, Request, Response, Status}
import org.http4s.rho.Result

import scala.reflect.runtime.universe.{Type, WeakTypeTag}

trait ResultMatcher[F[_], R] {
  def encodings: Set[MediaType]
  def resultInfo: Set[ResultInfo]

  def conv(req: Request[F], r: R)(implicit F: Monad[F], w: EntityEncoder[F, R]): F[Response[F]]
}

object ResultMatcher {

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

    /* Allowing the `Writable` to be `null` only matches real results but allows for
       situations where you return the same status with two types */
    implicit def maybeIsWritable[F[_], T](implicit t: WeakTypeTag[T], w: EntityEncoder[F, T] = null): MaybeWritable[T] = new MaybeWritable[T] {
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
  NETWORKAUTHENTICATIONREQUIRED](implicit mCONTINUE: MaybeWritable[CONTINUE],
                                mSWITCHINGPROTOCOLS: MaybeWritable[SWITCHINGPROTOCOLS],
                                        mPROCESSING: MaybeWritable[PROCESSING],
                                                mOK: MaybeWritable[OK],
                                           mCREATED: MaybeWritable[CREATED],
                                          mACCEPTED: MaybeWritable[ACCEPTED],
                       mNONAUTHORITATIVEINFORMATION: MaybeWritable[NONAUTHORITATIVEINFORMATION],
                                         mNOCONTENT: MaybeWritable[NOCONTENT],
                                      mRESETCONTENT: MaybeWritable[RESETCONTENT],
                                    mPARTIALCONTENT: MaybeWritable[PARTIALCONTENT],
                                       mMULTISTATUS: MaybeWritable[MULTISTATUS],
                                   mALREADYREPORTED: MaybeWritable[ALREADYREPORTED],
                                            mIMUSED: MaybeWritable[IMUSED],
                                   mMULTIPLECHOICES: MaybeWritable[MULTIPLECHOICES],
                                  mMOVEDPERMANENTLY: MaybeWritable[MOVEDPERMANENTLY],
                                             mFOUND: MaybeWritable[FOUND],
                                          mSEEOTHER: MaybeWritable[SEEOTHER],
                                       mNOTMODIFIED: MaybeWritable[NOTMODIFIED],
                                          mUSEPROXY: MaybeWritable[USEPROXY],
                                 mTEMPORARYREDIRECT: MaybeWritable[TEMPORARYREDIRECT],
                                 mPERMANENTREDIRECT: MaybeWritable[PERMANENTREDIRECT],
                                        mBADREQUEST: MaybeWritable[BADREQUEST],
                                      mUNAUTHORIZED: MaybeWritable[UNAUTHORIZED],
                                   mPAYMENTREQUIRED: MaybeWritable[PAYMENTREQUIRED],
                                         mFORBIDDEN: MaybeWritable[FORBIDDEN],
                                          mNOTFOUND: MaybeWritable[NOTFOUND],
                                  mMETHODNOTALLOWED: MaybeWritable[METHODNOTALLOWED],
                                     mNOTACCEPTABLE: MaybeWritable[NOTACCEPTABLE],
                       mPROXYAUTHENTICATIONREQUIRED: MaybeWritable[PROXYAUTHENTICATIONREQUIRED],
                                    mREQUESTTIMEOUT: MaybeWritable[REQUESTTIMEOUT],
                                          mCONFLICT: MaybeWritable[CONFLICT],
                                              mGONE: MaybeWritable[GONE],
                                    mLENGTHREQUIRED: MaybeWritable[LENGTHREQUIRED],
                                mPRECONDITIONFAILED: MaybeWritable[PRECONDITIONFAILED],
                                   mPAYLOADTOOLARGE: MaybeWritable[PAYLOADTOOLARGE],
                                        mURITOOLONG: MaybeWritable[URITOOLONG],
                              mUNSUPPORTEDMEDIATYPE: MaybeWritable[UNSUPPORTEDMEDIATYPE],
                               mRANGENOTSATISFIABLE: MaybeWritable[RANGENOTSATISFIABLE],
                                 mEXPECTATIONFAILED: MaybeWritable[EXPECTATIONFAILED],
                               mUNPROCESSABLEENTITY: MaybeWritable[UNPROCESSABLEENTITY],
                                            mLOCKED: MaybeWritable[LOCKED],
                                  mFAILEDDEPENDENCY: MaybeWritable[FAILEDDEPENDENCY],
                                   mUPGRADEREQUIRED: MaybeWritable[UPGRADEREQUIRED],
                              mPRECONDITIONREQUIRED: MaybeWritable[PRECONDITIONREQUIRED],
                                   mTOOMANYREQUESTS: MaybeWritable[TOOMANYREQUESTS],
                       mREQUESTHEADERFIELDSTOOLARGE: MaybeWritable[REQUESTHEADERFIELDSTOOLARGE],
                               mINTERNALSERVERERROR: MaybeWritable[INTERNALSERVERERROR],
                                    mNOTIMPLEMENTED: MaybeWritable[NOTIMPLEMENTED],
                                        mBADGATEWAY: MaybeWritable[BADGATEWAY],
                                mSERVICEUNAVAILABLE: MaybeWritable[SERVICEUNAVAILABLE],
                                    mGATEWAYTIMEOUT: MaybeWritable[GATEWAYTIMEOUT],
                           mHTTPVERSIONNOTSUPPORTED: MaybeWritable[HTTPVERSIONNOTSUPPORTED],
                             mVARIANTALSONEGOTIATES: MaybeWritable[VARIANTALSONEGOTIATES],
                               mINSUFFICIENTSTORAGE: MaybeWritable[INSUFFICIENTSTORAGE],
                                      mLOOPDETECTED: MaybeWritable[LOOPDETECTED],
                                       mNOTEXTENDED: MaybeWritable[NOTEXTENDED],
                     mNETWORKAUTHENTICATIONREQUIRED: MaybeWritable[NETWORKAUTHENTICATIONREQUIRED]): ResultMatcher[F, Result[
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
      NETWORKAUTHENTICATIONREQUIRED])(implicit F: Monad[F], w: EntityEncoder[F, Result[F, CONTINUE, SWITCHINGPROTOCOLS, PROCESSING, OK, CREATED, ACCEPTED, NONAUTHORITATIVEINFORMATION, NOCONTENT, RESETCONTENT, PARTIALCONTENT, MULTISTATUS, ALREADYREPORTED, IMUSED, MULTIPLECHOICES, MOVEDPERMANENTLY, FOUND, SEEOTHER, NOTMODIFIED, USEPROXY, TEMPORARYREDIRECT, PERMANENTREDIRECT, BADREQUEST, UNAUTHORIZED, PAYMENTREQUIRED, FORBIDDEN, NOTFOUND, METHODNOTALLOWED, NOTACCEPTABLE, PROXYAUTHENTICATIONREQUIRED, REQUESTTIMEOUT, CONFLICT, GONE, LENGTHREQUIRED, PRECONDITIONFAILED, PAYLOADTOOLARGE, URITOOLONG, UNSUPPORTEDMEDIATYPE, RANGENOTSATISFIABLE, EXPECTATIONFAILED, UNPROCESSABLEENTITY, LOCKED, FAILEDDEPENDENCY, UPGRADEREQUIRED, PRECONDITIONREQUIRED, TOOMANYREQUESTS, REQUESTHEADERFIELDSTOOLARGE, INTERNALSERVERERROR, NOTIMPLEMENTED, BADGATEWAY, SERVICEUNAVAILABLE, GATEWAYTIMEOUT, HTTPVERSIONNOTSUPPORTED, VARIANTALSONEGOTIATES, INSUFFICIENTSTORAGE, LOOPDETECTED, NOTEXTENDED, NETWORKAUTHENTICATIONREQUIRED]]): F[Response[F]] = F.pure(r.resp)

    override def resultInfo: Set[ResultInfo] = {
      allTpes.flatMap { case (s, mw) =>
        mw.resultInfo.map( t => StatusAndType(s, t))
      }.toSet
    }

    private lazy val allTpes: List[(Status, MaybeWritable[_])] = {
      import Status._
      List(
        (Continue, mCONTINUE),
        (SwitchingProtocols, mSWITCHINGPROTOCOLS),
        (Processing, mPROCESSING),
        (Ok, mOK),
        (Created, mCREATED),
        (Accepted, mACCEPTED),
        (NonAuthoritativeInformation, mNONAUTHORITATIVEINFORMATION),
        (NoContent, mNOCONTENT),
        (ResetContent, mRESETCONTENT),
        (PartialContent, mPARTIALCONTENT),
        (MultiStatus, mMULTISTATUS),
        (AlreadyReported, mALREADYREPORTED),
        (IMUsed, mIMUSED),
        (MultipleChoices, mMULTIPLECHOICES),
        (MovedPermanently, mMOVEDPERMANENTLY),
        (Found, mFOUND),
        (SeeOther, mSEEOTHER),
        (NotModified, mNOTMODIFIED),
        (UseProxy, mUSEPROXY),
        (TemporaryRedirect, mTEMPORARYREDIRECT),
        (PermanentRedirect, mPERMANENTREDIRECT),
        (BadRequest, mBADREQUEST),
        (Unauthorized, mUNAUTHORIZED),
        (PaymentRequired, mPAYMENTREQUIRED),
        (Forbidden, mFORBIDDEN),
        (NotFound, mNOTFOUND),
        (MethodNotAllowed, mMETHODNOTALLOWED),
        (NotAcceptable, mNOTACCEPTABLE),
        (ProxyAuthenticationRequired, mPROXYAUTHENTICATIONREQUIRED),
        (RequestTimeout, mREQUESTTIMEOUT),
        (Conflict, mCONFLICT),
        (Gone, mGONE),
        (LengthRequired, mLENGTHREQUIRED),
        (PreconditionFailed, mPRECONDITIONFAILED),
        (PayloadTooLarge, mPAYLOADTOOLARGE),
        (UriTooLong, mURITOOLONG),
        (UnsupportedMediaType, mUNSUPPORTEDMEDIATYPE),
        (RangeNotSatisfiable, mRANGENOTSATISFIABLE),
        (ExpectationFailed, mEXPECTATIONFAILED),
        (UnprocessableEntity, mUNPROCESSABLEENTITY),
        (Locked, mLOCKED),
        (FailedDependency, mFAILEDDEPENDENCY),
        (UpgradeRequired, mUPGRADEREQUIRED),
        (PreconditionRequired, mPRECONDITIONREQUIRED),
        (TooManyRequests, mTOOMANYREQUESTS),
        (RequestHeaderFieldsTooLarge, mREQUESTHEADERFIELDSTOOLARGE),
        (InternalServerError, mINTERNALSERVERERROR),
        (NotImplemented, mNOTIMPLEMENTED),
        (BadGateway, mBADGATEWAY),
        (ServiceUnavailable, mSERVICEUNAVAILABLE),
        (GatewayTimeout, mGATEWAYTIMEOUT),
        (HttpVersionNotSupported, mHTTPVERSIONNOTSUPPORTED),
        (VariantAlsoNegotiates, mVARIANTALSONEGOTIATES),
        (InsufficientStorage, mINSUFFICIENTSTORAGE),
        (LoopDetected, mLOOPDETECTED),
        (NotExtended, mNOTEXTENDED),
        (NetworkAuthenticationRequired, mNETWORKAUTHENTICATIONREQUIRED)
      )
    }
  }

  implicit def optionMatcher[F[_], R](implicit o: WeakTypeTag[R], w: EntityEncoder[F, Option[R]]): ResultMatcher[F, Option[R]] = new ResultMatcher[F, Option[R]] {
    override val encodings: Set[MediaType] = w.contentType.map(_.mediaType).toSet
    override val resultInfo: Set[ResultInfo] = Set(StatusAndType(Status.Ok, o.tpe.dealias),
                                                   StatusOnly(Status.NotFound))

    override def conv(req: Request[F], r: Option[R])(implicit F: Monad[F], w: EntityEncoder[F, Option[R]]): F[Response[F]] = r match {
      case Some(`r`) => ResponseGeneratorInstances.Ok[F].pure(r)
      case None      => ResponseGeneratorInstances.NotFound[F].pure(req.uri.path)
    }
  }

  implicit def writableMatcher[F[_], R](implicit o: WeakTypeTag[R], w: EntityEncoder[F, R]): ResultMatcher[F, R] = new ResultMatcher[F, R] {
    override def encodings: Set[MediaType] = w.contentType.map(_.mediaType).toSet
    override def resultInfo: Set[ResultInfo] = Set(StatusAndType(Status.Ok, o.tpe.dealias))

    override def conv(req: Request[F], r: R)(implicit F: Monad[F], w: EntityEncoder[F, R]): F[Response[F]] = ResponseGeneratorInstances.Ok[F].pure(r)
  }

  implicit def fMatcher[F[_], R](implicit F: FlatMap[F], r: ResultMatcher[F, R]): ResultMatcher[F, F[R]] = new ResultMatcher[F, F[R]] {
    override def encodings: Set[MediaType] = r.encodings
    override def resultInfo: Set[ResultInfo] = r.resultInfo

    override def conv(req: Request[F], t: F[R])(implicit F: Monad[F], w: EntityEncoder[F, F[R]]): F[Response[F]] = F.flatMap(t)(r.conv(req, _))
  }

  implicit def responseMatcher[F[_]](implicit F: Applicative[F]): ResultMatcher[F, Response[F]] = new ResultMatcher[F, Response[F]] {
    override def encodings: Set[MediaType] = Set.empty
    override def resultInfo: Set[ResultInfo] = Set.empty

    override def conv(req: Request[F], r: Response[F])(implicit F: Monad[F], w: EntityEncoder[F, Response[F]]): F[Response[F]] = F.pure(r)
  }
}
