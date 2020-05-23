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

trait ResultMatchers[F[_]] extends ResultMatcherMidPrioInstances[F] {

  sealed trait MaybeWritable[T] {
    def contentType: Set[MediaType]
    def resultInfo: Option[Type]
    def encodings: Set[MediaType]
  }

  object MaybeWritable extends MaybeWritableOps {
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
    implicit def maybeIsWritable[T](implicit t: WeakTypeTag[T], w: EntityEncoder[F, T] = null): MaybeWritable[T] = new MaybeWritable[T] {
      private val ww = Option(w)
      override def contentType: Set[MediaType] = ww.flatMap(_.contentType.map(_.mediaType)).toSet
      override def encodings: Set[MediaType] = ww.flatMap(_.contentType.map(_.mediaType)).toSet
      override def resultInfo: Option[Type] = Some(t.tpe.dealias)
    }
  }

  implicit def statusMatcher[
  /* 100 */ CONTINUE,
  /* 101 */ SWITCHINGPROTOCOLS,
  /* 102 */ PROCESSING,
  /* 103 */ EARLYHINTS,

  /* 200 */ OK,
  /* 201 */ CREATED,
  /* 202 */ ACCEPTED,
  /* 203 */ NONAUTHORITATIVEINFORMATION,
  /* 204 */ NOCONTENT,
  /* 205 */ RESETCONTENT,
  /* 206 */ PARTIALCONTENT,
  /* 207 */ MULTISTATUS,
  /* 208 */ ALREADYREPORTED,
  /* 226 */ IMUSED,

  /* 300 */ MULTIPLECHOICES,
  /* 301 */ MOVEDPERMANENTLY,
  /* 302 */ FOUND,
  /* 303 */ SEEOTHER,
  /* 304 */ NOTMODIFIED,
  /* 307 */ TEMPORARYREDIRECT,
  /* 308 */ PERMANENTREDIRECT,

  /* 400 */ BADREQUEST,
  /* 401 */ UNAUTHORIZED,
  /* 402 */ PAYMENTREQUIRED,
  /* 403 */ FORBIDDEN,
  /* 404 */ NOTFOUND,
  /* 405 */ METHODNOTALLOWED,
  /* 406 */ NOTACCEPTABLE,
  /* 407 */ PROXYAUTHENTICATIONREQUIRED,
  /* 408 */ REQUESTTIMEOUT,
  /* 409 */ CONFLICT,
  /* 410 */ GONE,
  /* 411 */ LENGTHREQUIRED,
  /* 412 */ PRECONDITIONFAILED,
  /* 413 */ PAYLOADTOOLARGE,
  /* 414 */ URITOOLONG,
  /* 415 */ UNSUPPORTEDMEDIATYPE,
  /* 416 */ RANGENOTSATISFIABLE,
  /* 417 */ EXPECTATIONFAILED,
  /* 421 */ MISDIRECTEDREQUEST,
  /* 422 */ UNPROCESSABLEENTITY,
  /* 423 */ LOCKED,
  /* 424 */ FAILEDDEPENDENCY,
  /* 424 */ TOOEARLY,
  /* 426 */ UPGRADEREQUIRED,
  /* 428 */ PRECONDITIONREQUIRED,
  /* 429 */ TOOMANYREQUESTS,
  /* 431 */ REQUESTHEADERFIELDSTOOLARGE,
  /* 451 */ UNAVAILABLEFORLEGALREASONS,

  /* 500 */ INTERNALSERVERERROR,
  /* 501 */ NOTIMPLEMENTED,
  /* 502 */ BADGATEWAY,
  /* 503 */ SERVICEUNAVAILABLE,
  /* 504 */ GATEWAYTIMEOUT,
  /* 505 */ HTTPVERSIONNOTSUPPORTED,
  /* 506 */ VARIANTALSONEGOTIATES,
  /* 507 */ INSUFFICIENTSTORAGE,
  /* 508 */ LOOPDETECTED,
  /* 510 */ NOTEXTENDED,
  /* 511 */ NETWORKAUTHENTICATIONREQUIRED
  ](implicit
    /* 100 */ mCONTINUE: MaybeWritable[CONTINUE],
    /* 101 */ mSWITCHINGPROTOCOLS: MaybeWritable[SWITCHINGPROTOCOLS],
    /* 102 */ mPROCESSING: MaybeWritable[PROCESSING],
    /* 103 */ mEARLYHINTS: MaybeWritable[EARLYHINTS],
    /* 200 */ mOK: MaybeWritable[OK],
    /* 201 */ mCREATED: MaybeWritable[CREATED],
    /* 202 */ mACCEPTED: MaybeWritable[ACCEPTED],
    /* 203 */ mNONAUTHORITATIVEINFORMATION: MaybeWritable[NONAUTHORITATIVEINFORMATION],
    /* 204 */ mNOCONTENT: MaybeWritable[NOCONTENT],
    /* 205 */ mRESETCONTENT: MaybeWritable[RESETCONTENT],
    /* 206 */ mPARTIALCONTENT: MaybeWritable[PARTIALCONTENT],
    /* 207 */ mMULTISTATUS: MaybeWritable[MULTISTATUS],
    /* 208 */ mALREADYREPORTED: MaybeWritable[ALREADYREPORTED],
    /* 226 */ mIMUSED: MaybeWritable[IMUSED],
    /* 300 */ mMULTIPLECHOICES: MaybeWritable[MULTIPLECHOICES],
    /* 301 */ mMOVEDPERMANENTLY: MaybeWritable[MOVEDPERMANENTLY],
    /* 302 */ mFOUND: MaybeWritable[FOUND],
    /* 303 */ mSEEOTHER: MaybeWritable[SEEOTHER],
    /* 304 */ mNOTMODIFIED: MaybeWritable[NOTMODIFIED],
    /* 307 */ mTEMPORARYREDIRECT: MaybeWritable[TEMPORARYREDIRECT],
    /* 308 */ mPERMANENTREDIRECT: MaybeWritable[PERMANENTREDIRECT],
    /* 400 */ mBADREQUEST: MaybeWritable[BADREQUEST],
    /* 401 */ mUNAUTHORIZED: MaybeWritable[UNAUTHORIZED],
    /* 402 */ mPAYMENTREQUIRED: MaybeWritable[PAYMENTREQUIRED],
    /* 403 */ mFORBIDDEN: MaybeWritable[FORBIDDEN],
    /* 404 */ mNOTFOUND: MaybeWritable[NOTFOUND],
    /* 405 */ mMETHODNOTALLOWED: MaybeWritable[METHODNOTALLOWED],
    /* 406 */ mNOTACCEPTABLE: MaybeWritable[NOTACCEPTABLE],
    /* 407 */ mPROXYAUTHENTICATIONREQUIRED: MaybeWritable[PROXYAUTHENTICATIONREQUIRED],
    /* 408 */ mREQUESTTIMEOUT: MaybeWritable[REQUESTTIMEOUT],
    /* 409 */ mCONFLICT: MaybeWritable[CONFLICT],
    /* 410 */ mGONE: MaybeWritable[GONE],
    /* 411 */ mLENGTHREQUIRED: MaybeWritable[LENGTHREQUIRED],
    /* 412 */ mPRECONDITIONFAILED: MaybeWritable[PRECONDITIONFAILED],
    /* 413 */ mPAYLOADTOOLARGE: MaybeWritable[PAYLOADTOOLARGE],
    /* 414 */ mURITOOLONG: MaybeWritable[URITOOLONG],
    /* 415 */ mUNSUPPORTEDMEDIATYPE: MaybeWritable[UNSUPPORTEDMEDIATYPE],
    /* 416 */ mRANGENOTSATISFIABLE: MaybeWritable[RANGENOTSATISFIABLE],
    /* 417 */ mEXPECTATIONFAILED: MaybeWritable[EXPECTATIONFAILED],
    /* 421 */ mMISDIRECTEDREQUEST: MaybeWritable[MISDIRECTEDREQUEST],
    /* 422 */ mUNPROCESSABLEENTITY: MaybeWritable[UNPROCESSABLEENTITY],
    /* 423 */ mLOCKED: MaybeWritable[LOCKED],
    /* 424 */ mFAILEDDEPENDENCY: MaybeWritable[FAILEDDEPENDENCY],
    /* 424 */ mTOOEARLY: MaybeWritable[TOOEARLY],
    /* 426 */ mUPGRADEREQUIRED: MaybeWritable[UPGRADEREQUIRED],
    /* 428 */ mPRECONDITIONREQUIRED: MaybeWritable[PRECONDITIONREQUIRED],
    /* 429 */ mTOOMANYREQUESTS: MaybeWritable[TOOMANYREQUESTS],
    /* 431 */ mREQUESTHEADERFIELDSTOOLARGE: MaybeWritable[REQUESTHEADERFIELDSTOOLARGE],
    /* 451 */ mUNAVAILABLEFORLEGALREASONS: MaybeWritable[UNAVAILABLEFORLEGALREASONS],
    /* 500 */ mINTERNALSERVERERROR: MaybeWritable[INTERNALSERVERERROR],
    /* 501 */ mNOTIMPLEMENTED: MaybeWritable[NOTIMPLEMENTED],
    /* 502 */ mBADGATEWAY: MaybeWritable[BADGATEWAY],
    /* 503 */ mSERVICEUNAVAILABLE: MaybeWritable[SERVICEUNAVAILABLE],
    /* 504 */ mGATEWAYTIMEOUT: MaybeWritable[GATEWAYTIMEOUT],
    /* 505 */ mHTTPVERSIONNOTSUPPORTED: MaybeWritable[HTTPVERSIONNOTSUPPORTED],
    /* 506 */ mVARIANTALSONEGOTIATES: MaybeWritable[VARIANTALSONEGOTIATES],
    /* 507 */ mINSUFFICIENTSTORAGE: MaybeWritable[INSUFFICIENTSTORAGE],
    /* 508 */ mLOOPDETECTED: MaybeWritable[LOOPDETECTED],
    /* 510 */ mNOTEXTENDED: MaybeWritable[NOTEXTENDED],
    /* 511 */ mNETWORKAUTHENTICATIONREQUIRED: MaybeWritable[NETWORKAUTHENTICATIONREQUIRED]
   ): ResultMatcher[F, Result[
    F,
    /* 100 */ CONTINUE,
    /* 101 */ SWITCHINGPROTOCOLS,
    /* 102 */ PROCESSING,
    /* 103 */ EARLYHINTS,

    /* 200 */ OK,
    /* 201 */ CREATED,
    /* 202 */ ACCEPTED,
    /* 203 */ NONAUTHORITATIVEINFORMATION,
    /* 204 */ NOCONTENT,
    /* 205 */ RESETCONTENT,
    /* 206 */ PARTIALCONTENT,
    /* 207 */ MULTISTATUS,
    /* 208 */ ALREADYREPORTED,
    /* 226 */ IMUSED,

    /* 300 */ MULTIPLECHOICES,
    /* 301 */ MOVEDPERMANENTLY,
    /* 302 */ FOUND,
    /* 303 */ SEEOTHER,
    /* 304 */ NOTMODIFIED,
    /* 307 */ TEMPORARYREDIRECT,
    /* 308 */ PERMANENTREDIRECT,

    /* 400 */ BADREQUEST,
    /* 401 */ UNAUTHORIZED,
    /* 402 */ PAYMENTREQUIRED,
    /* 403 */ FORBIDDEN,
    /* 404 */ NOTFOUND,
    /* 405 */ METHODNOTALLOWED,
    /* 406 */ NOTACCEPTABLE,
    /* 407 */ PROXYAUTHENTICATIONREQUIRED,
    /* 408 */ REQUESTTIMEOUT,
    /* 409 */ CONFLICT,
    /* 410 */ GONE,
    /* 411 */ LENGTHREQUIRED,
    /* 412 */ PRECONDITIONFAILED,
    /* 413 */ PAYLOADTOOLARGE,
    /* 414 */ URITOOLONG,
    /* 415 */ UNSUPPORTEDMEDIATYPE,
    /* 416 */ RANGENOTSATISFIABLE,
    /* 417 */ EXPECTATIONFAILED,
    /* 421 */ MISDIRECTEDREQUEST,
    /* 422 */ UNPROCESSABLEENTITY,
    /* 423 */ LOCKED,
    /* 424 */ FAILEDDEPENDENCY,
    /* 424 */ TOOEARLY,
    /* 426 */ UPGRADEREQUIRED,
    /* 428 */ PRECONDITIONREQUIRED,
    /* 429 */ TOOMANYREQUESTS,
    /* 431 */ REQUESTHEADERFIELDSTOOLARGE,
    /* 451 */ UNAVAILABLEFORLEGALREASONS,

    /* 500 */ INTERNALSERVERERROR,
    /* 501 */ NOTIMPLEMENTED,
    /* 502 */ BADGATEWAY,
    /* 503 */ SERVICEUNAVAILABLE,
    /* 504 */ GATEWAYTIMEOUT,
    /* 505 */ HTTPVERSIONNOTSUPPORTED,
    /* 506 */ VARIANTALSONEGOTIATES,
    /* 507 */ INSUFFICIENTSTORAGE,
    /* 508 */ LOOPDETECTED,
    /* 510 */ NOTEXTENDED,
    /* 511 */ NETWORKAUTHENTICATIONREQUIRED]] =
  new ResultMatcher[F, Result[
    F,
    /* 100 */ CONTINUE,
    /* 101 */ SWITCHINGPROTOCOLS,
    /* 102 */ PROCESSING,
    /* 103 */ EARLYHINTS,

    /* 200 */ OK,
    /* 201 */ CREATED,
    /* 202 */ ACCEPTED,
    /* 203 */ NONAUTHORITATIVEINFORMATION,
    /* 204 */ NOCONTENT,
    /* 205 */ RESETCONTENT,
    /* 206 */ PARTIALCONTENT,
    /* 207 */ MULTISTATUS,
    /* 208 */ ALREADYREPORTED,
    /* 226 */ IMUSED,

    /* 300 */ MULTIPLECHOICES,
    /* 301 */ MOVEDPERMANENTLY,
    /* 302 */ FOUND,
    /* 303 */ SEEOTHER,
    /* 304 */ NOTMODIFIED,
    /* 307 */ TEMPORARYREDIRECT,
    /* 308 */ PERMANENTREDIRECT,

    /* 400 */ BADREQUEST,
    /* 401 */ UNAUTHORIZED,
    /* 402 */ PAYMENTREQUIRED,
    /* 403 */ FORBIDDEN,
    /* 404 */ NOTFOUND,
    /* 405 */ METHODNOTALLOWED,
    /* 406 */ NOTACCEPTABLE,
    /* 407 */ PROXYAUTHENTICATIONREQUIRED,
    /* 408 */ REQUESTTIMEOUT,
    /* 409 */ CONFLICT,
    /* 410 */ GONE,
    /* 411 */ LENGTHREQUIRED,
    /* 412 */ PRECONDITIONFAILED,
    /* 413 */ PAYLOADTOOLARGE,
    /* 414 */ URITOOLONG,
    /* 415 */ UNSUPPORTEDMEDIATYPE,
    /* 416 */ RANGENOTSATISFIABLE,
    /* 417 */ EXPECTATIONFAILED,
    /* 421 */ MISDIRECTEDREQUEST,
    /* 422 */ UNPROCESSABLEENTITY,
    /* 423 */ LOCKED,
    /* 424 */ FAILEDDEPENDENCY,
    /* 424 */ TOOEARLY,
    /* 426 */ UPGRADEREQUIRED,
    /* 428 */ PRECONDITIONREQUIRED,
    /* 429 */ TOOMANYREQUESTS,
    /* 431 */ REQUESTHEADERFIELDSTOOLARGE,
    /* 451 */ UNAVAILABLEFORLEGALREASONS,

    /* 500 */ INTERNALSERVERERROR,
    /* 501 */ NOTIMPLEMENTED,
    /* 502 */ BADGATEWAY,
    /* 503 */ SERVICEUNAVAILABLE,
    /* 504 */ GATEWAYTIMEOUT,
    /* 505 */ HTTPVERSIONNOTSUPPORTED,
    /* 506 */ VARIANTALSONEGOTIATES,
    /* 507 */ INSUFFICIENTSTORAGE,
    /* 508 */ LOOPDETECTED,
    /* 510 */ NOTEXTENDED,
    /* 511 */ NETWORKAUTHENTICATIONREQUIRED]] {
    override lazy val encodings: Set[MediaType] =
      allTpes.flatMap { case (_, m) => m.encodings }.toSet

    override def conv(req: Request[F], r: Result[
      F,
      /* 100 */ CONTINUE,
      /* 101 */ SWITCHINGPROTOCOLS,
      /* 102 */ PROCESSING,
      /* 103 */ EARLYHINTS,

      /* 200 */ OK,
      /* 201 */ CREATED,
      /* 202 */ ACCEPTED,
      /* 203 */ NONAUTHORITATIVEINFORMATION,
      /* 204 */ NOCONTENT,
      /* 205 */ RESETCONTENT,
      /* 206 */ PARTIALCONTENT,
      /* 207 */ MULTISTATUS,
      /* 208 */ ALREADYREPORTED,
      /* 226 */ IMUSED,

      /* 300 */ MULTIPLECHOICES,
      /* 301 */ MOVEDPERMANENTLY,
      /* 302 */ FOUND,
      /* 303 */ SEEOTHER,
      /* 304 */ NOTMODIFIED,
      /* 307 */ TEMPORARYREDIRECT,
      /* 308 */ PERMANENTREDIRECT,

      /* 400 */ BADREQUEST,
      /* 401 */ UNAUTHORIZED,
      /* 402 */ PAYMENTREQUIRED,
      /* 403 */ FORBIDDEN,
      /* 404 */ NOTFOUND,
      /* 405 */ METHODNOTALLOWED,
      /* 406 */ NOTACCEPTABLE,
      /* 407 */ PROXYAUTHENTICATIONREQUIRED,
      /* 408 */ REQUESTTIMEOUT,
      /* 409 */ CONFLICT,
      /* 410 */ GONE,
      /* 411 */ LENGTHREQUIRED,
      /* 412 */ PRECONDITIONFAILED,
      /* 413 */ PAYLOADTOOLARGE,
      /* 414 */ URITOOLONG,
      /* 415 */ UNSUPPORTEDMEDIATYPE,
      /* 416 */ RANGENOTSATISFIABLE,
      /* 417 */ EXPECTATIONFAILED,
      /* 421 */ MISDIRECTEDREQUEST,
      /* 422 */ UNPROCESSABLEENTITY,
      /* 423 */ LOCKED,
      /* 424 */ FAILEDDEPENDENCY,
      /* 424 */ TOOEARLY,
      /* 426 */ UPGRADEREQUIRED,
      /* 428 */ PRECONDITIONREQUIRED,
      /* 429 */ TOOMANYREQUESTS,
      /* 431 */ REQUESTHEADERFIELDSTOOLARGE,
      /* 451 */ UNAVAILABLEFORLEGALREASONS,

      /* 500 */ INTERNALSERVERERROR,
      /* 501 */ NOTIMPLEMENTED,
      /* 502 */ BADGATEWAY,
      /* 503 */ SERVICEUNAVAILABLE,
      /* 504 */ GATEWAYTIMEOUT,
      /* 505 */ HTTPVERSIONNOTSUPPORTED,
      /* 506 */ VARIANTALSONEGOTIATES,
      /* 507 */ INSUFFICIENTSTORAGE,
      /* 508 */ LOOPDETECTED,
      /* 510 */ NOTEXTENDED,
      /* 511 */ NETWORKAUTHENTICATIONREQUIRED])(implicit F: Monad[F]): F[Response[F]] = F.pure(r.resp)

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
        (org.http4s.Status.EarlyHints, mEARLYHINTS),
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
        (org.http4s.Status.MisdirectedRequest, mMISDIRECTEDREQUEST),
        (org.http4s.Status.UnprocessableEntity, mUNPROCESSABLEENTITY),
        (org.http4s.Status.Locked, mLOCKED),
        (org.http4s.Status.FailedDependency, mFAILEDDEPENDENCY),
        (org.http4s.Status.TooEarly, mTOOEARLY),
        (org.http4s.Status.UpgradeRequired, mUPGRADEREQUIRED),
        (org.http4s.Status.PreconditionRequired, mPRECONDITIONREQUIRED),
        (org.http4s.Status.TooManyRequests, mTOOMANYREQUESTS),
        (org.http4s.Status.RequestHeaderFieldsTooLarge, mREQUESTHEADERFIELDSTOOLARGE),
        (org.http4s.Status.UnavailableForLegalReasons, mUNAVAILABLEFORLEGALREASONS),
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

  implicit def optionMatcher[R](implicit o: WeakTypeTag[R], w: EntityEncoder[F, R]): ResultMatcher[F, Option[R]] = new ResultMatcher[F, Option[R]] with ResponseGeneratorInstances[F] {
    override val encodings: Set[MediaType] = w.contentType.map(_.mediaType).toSet
    override val resultInfo: Set[ResultInfo] = Set(StatusAndType(Status.Ok, o.tpe.dealias),
                                                   StatusOnly(Status.NotFound))

    override def conv(req: Request[F], r: Option[R])(implicit F: Monad[F]): F[Response[F]] = r match {
      case Some(res) => Ok.pure(res)
      case None      => NotFound.pure(req.uri.path)
    }
  }

  implicit def responseMatcher: ResultMatcher[F, Response[F]] = new ResultMatcher[F, Response[F]] {
    override def encodings: Set[MediaType] = Set.empty
    override def resultInfo: Set[ResultInfo] = Set.empty

    override def conv(req: Request[F], r: Response[F])(implicit F: Monad[F]): F[Response[F]] = F.pure(r)
  }
}

trait ResultMatcherMidPrioInstances[F[_]] extends ResultMatcherLowPrioInstances[F] {
  implicit def fMatcher[R](implicit r: ResultMatcher[F, R]): ResultMatcher[F, F[R]] = new ResultMatcher[F, F[R]] {
    override def encodings: Set[MediaType] = r.encodings
    override def resultInfo: Set[ResultInfo] = r.resultInfo

    override def conv(req: Request[F], f: F[R])(implicit F: Monad[F]): F[Response[F]] = F.flatMap(f)(r.conv(req, _))
  }
}

trait ResultMatcherLowPrioInstances[F[_]] {
  implicit def writableMatcher[R](implicit o: WeakTypeTag[R], w: EntityEncoder[F, R]): ResultMatcher[F, R] = new ResultMatcher[F, R] with ResponseGeneratorInstances[F] {
    override def encodings: Set[MediaType] = w.contentType.map(_.mediaType).toSet
    override def resultInfo: Set[ResultInfo] = Set(StatusAndType(Status.Ok, o.tpe.dealias))

    override def conv(req: Request[F], r: R)(implicit F: Monad[F]): F[Response[F]] = Ok.pure(r)
  }
}
