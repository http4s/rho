package org.http4s.rho

import cats.syntax.functor._
import cats.Monad
import org.http4s._
import org.http4s.rho.Result.BaseResult
import org.http4s.rho.bits.RequestAST.CaptureRule
import org.http4s.rho.bits._
import org.log4s.{Logger, getLogger}
import shapeless.{::, HNil}

import scala.util.control.NonFatal

trait RhoDslHeaderExtractors[F[_]]
  extends FailureResponseOps[F] {

  private[this] val logger: Logger = getLogger

  /** Requires that the header exists
    *
    * @param header `HeaderKey` that identifies the header which is required
    */
  def exists(header: HeaderKey.Extractable)(implicit F: Monad[F]): TypedHeader[F, HNil] = existsAndR(header)(_ => None)

  /** Requires that the header exists and satisfies the condition
    *
    * @param header  `HeaderKey` that identifies the header to capture and parse
    * @param f predicate function where a return value of `false` signals an invalid
    *          header and aborts evaluation with a _BadRequest_ response.
    */
  def existsAnd[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Boolean)(implicit F: Monad[F]): TypedHeader[F, HNil] =
    existsAndR[H](header){ h =>
      if (f(h)) None
      else Some(invalidHeaderResponse(h))
    }

  /** Check that the header exists and satisfies the condition
    *
    * @param header `HeaderKey` that identifies the header to capture and parse
    * @param f function that evaluates the header and returns a Some(Response) to
    *          immediately send back to the user or None to continue evaluation.
    */
  def existsAndR[H <: HeaderKey.Extractable](header: H)(f: H#HeaderT => Option[F[BaseResult[F]]])(implicit F: Monad[F]): TypedHeader[F, HNil] =
    captureMapR(header, None){ h => f(h) match {
      case Some(r) => Left(r)
      case None    => Right(())
    }
  }.ignore


  /** Capture the header and put it into the function args stack, if it exists
    *
    * @param key `HeaderKey` used to identify the header to capture
    */
  def captureOptionally[H <: HeaderKey.Extractable](key: H)(implicit F: Monad[F]): TypedHeader[F, Option[H#HeaderT] :: HNil] =
    _captureMapR[H, Option[H#HeaderT]](key, isRequired = false)(Right(_))

  /** requires the header and will pull this header from the pile and put it into the function args stack
    *
    * @param key `HeaderKey` used to identify the header to capture
    */
  def capture[H <: HeaderKey.Extractable](key: H)(implicit F: Monad[F]): TypedHeader[F, H#HeaderT :: HNil] =
    captureMap(key)(identity)

  /** Capture the header and put it into the function args stack, if it exists otherwise put the default in the args stack
    *
    * @param key `HeaderKey` used to identify the header to capture
    * @param default The default to be used if the header was not present
    */
  def captureOrElse[H <: HeaderKey.Extractable](key: H)(default: H#HeaderT)(implicit F: Monad[F]): TypedHeader[F, H#HeaderT :: HNil] =
    captureOptionally[H](key).map { header: Option[H#HeaderT] =>
      header.getOrElse(default)
    }

  /** Capture a specific header and map its value
    *
    * @param key `HeaderKey` used to identify the header to capture
    * @param f mapping function
    */
  def captureMap[H <: HeaderKey.Extractable, R](key: H)(f: H#HeaderT => R)(implicit F: Monad[F]): TypedHeader[F, R :: HNil] =
    captureMapR(key, None, isRequired = true)(f andThen (Right(_)))

  /** Capture a specific header and map its value with an optional override of the missing header response
    *
    * @param key `HeaderKey` used to identify the header to capture
    * @param missingHeaderResult optional override result for the case of a missing header
    * @param isRequired indicates for metadata purposes that the header is required, always true if `missingHeaderResult` is unset
    * @param f mapping function
    */
  def captureMapR[H <: HeaderKey.Extractable, R](key: H, missingHeaderResult: Option[F[BaseResult[F]]] = None, isRequired: Boolean = false)(f: H#HeaderT => Either[F[BaseResult[F]], R])(implicit F: Monad[F]): TypedHeader[F, R :: HNil] =
    _captureMapR(key, missingHeaderResult, missingHeaderResult.isEmpty || isRequired)(f)

  /** Create a header capture rule using the `Request`'s `Headers`
    *
    * @param f function generating the result or failure
    */
  def genericHeaderCapture[R](f: Headers => ResultResponse[F, R]): TypedHeader[F, R :: HNil] =
    genericRequestHeaderCapture[R](req => f(req.headers))

  /** Create a header capture rule using the `Request`
    *
    * In general, this function should be avoided for most cases because it has access to the entire `Request`
    * which allows it to modify the `Request` body which should be avoided.
    *
    * @param f function generating the result or failure
    */
  def genericRequestHeaderCapture[R](f: Request[F] => ResultResponse[F, R]): TypedHeader[F, R :: HNil] =
    TypedHeader[F, R :: HNil](CaptureRule(f))

  private def _captureMapR[H <: HeaderKey.Extractable, R](key: H, missingHeaderResult: Option[F[BaseResult[F]]], isRequired: Boolean)(f: H#HeaderT => Either[F[BaseResult[F]], R])(implicit F: Monad[F]): TypedHeader[F, R :: HNil] =
    _captureMapR[H, R](key, isRequired) {
      case Some(header) => f(header)
      case None => Left(missingHeaderResult.getOrElse(missingHeaderResponse(key)))
    }

  private def _captureMapR[H <: HeaderKey.Extractable, R](key: H, isRequired: Boolean)(f: Option[H#HeaderT] => Either[F[BaseResult[F]], R])(implicit F: Monad[F]): TypedHeader[F, R :: HNil] =
    genericHeaderCapture[R] { headers =>
      val h = headers.get(key)
      try f(h) match {
        case Right(r) => SuccessResponse(r)
        case Left(r) => FailureResponse.result(r)
      } catch {
        case NonFatal(e) =>
          FailureResponse.result(errorProcessingHeaderResponse(key, h, e))
      }
    }.withMetadata(HeaderMetaData(key, isRequired = isRequired))

  protected def invalidHeaderResponse[H <: HeaderKey.Extractable](h: H#HeaderT)(implicit F: Monad[F]): F[BaseResult[F]] =
    BadRequest(s"Invalid header: ${h.name} = ${h.value}").widen

  protected def missingHeaderResponse[H <: HeaderKey](key: H)(implicit F: Monad[F]): F[BaseResult[F]] =
    BadRequest(s"Missing header: ${key.name}").widen[BaseResult[F]]

  protected def errorProcessingHeaderResponse[H <: HeaderKey.Extractable](key: H, header: Option[H#HeaderT], nonfatal: Throwable)(implicit F: Monad[F]): F[BaseResult[F]] = {
    logger.error(nonfatal)(s"""Failure during header capture: "${key.name}" ${header.fold("Undefined")(v => s"""= "${v.value}"""")}""")
    InternalServerError("Error processing request.").widen[BaseResult[F]]
  }
}

