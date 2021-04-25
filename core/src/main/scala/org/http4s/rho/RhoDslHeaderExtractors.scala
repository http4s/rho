package org.http4s.rho

import cats._
import cats.data._
import cats.implicits._
import org.http4s._
import org.http4s.rho.Result.BaseResult
import org.http4s.rho.bits.RequestAST.CaptureRule
import org.http4s.rho.bits._
import org.log4s.{Logger, getLogger}
import shapeless.{::, HNil}

import scala.util.control.NonFatal

trait RhoDslHeaderExtractors[F[_]] extends FailureResponseOps[F] {

  private[this] val logger: Logger = getLogger

  /** Create a header capture rule using the `Request`'s `Headers`
    *
    * In general, this function should be avoided because no metadata will be captured and
    * added to the swagger documention
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
  def genericRequestHeaderCapture[R](
      f: Request[F] => ResultResponse[F, R]): TypedHeader[F, R :: HNil] =
    TypedHeader[F, R :: HNil](CaptureRule(f))

  def H[A](implicit H: Header[A, _], S: Header.Select[A], F: Monad[F]): HeaderOps[A, S.F] =
    new HeaderOps[A, S.F]()(H, S, F)

  class HeaderOps[A, H[_]](implicit H: Header[A, _], S: Header.Select.Aux[A, H], F: Monad[F]) {
    type HR = H[A]

    /** Requires that the header exists
      */
    def exists: TypedHeader[F, HNil] =
      existsAndR(_ => None)

    /** Requires that the header exists and satisfies the condition
      *
      * @param f predicate function where a return value of `false` signals an invalid
      *          header and aborts evaluation with a _BadRequest_ response.
      */
    def existsAnd(f: HR => Boolean): TypedHeader[F, HNil] =
      existsAndR { (h: HR) =>
        if (f(h)) None
        else Some(invalidHeaderResponse[A])
      }

    /** Check that the header exists and satisfies the condition
      *
      * @param f function that evaluates the header and returns a Some(Response) to
      *          immediately send back to the user or None to continue evaluation.
      */
    def existsAndR(f: HR => Option[F[BaseResult[F]]]): TypedHeader[F, HNil] =
      captureMapR(None) { h =>
        f(h) match {
          case Some(r) => Left(r)
          case None => Right(())
        }
      }.ignore

    /** Capture the header and put it into the function args stack, if it exists
      */
    def captureOptionally: TypedHeader[F, Option[HR] :: HNil] =
      _captureMapR(isRequired = false)(SuccessResponse[F, Option[HR]](_))

    /** requires the header and will pull this header from the pile and put it into the function args stack
      */
    def capture: TypedHeader[F, HR :: HNil] =
      captureMap(identity)

    /** Capture the header and put it into the function args stack, if it exists otherwise put the default in the args stack
      *
      * @param default The default to be used if the header was not present
      */
    def captureOrElse(default: HR): TypedHeader[F, HR :: HNil] =
      captureOptionally.map { header: Option[HR] =>
        header.getOrElse(default)
      }

    /** Capture a specific header and map its value
      *
      * @param f mapping function
      */
    def captureMap[R](f: HR => R): TypedHeader[F, R :: HNil] =
      captureMapR[R](None, isRequired = true)(f.andThen(Right(_)))

    /** Capture a specific header and map its value with an optional override of the missing header response
      *
      * @param missingHeaderResult optional override result for the case of a missing header
      * @param isRequired indicates for metadata purposes that the header is required, always true if `missingHeaderResult` is unset
      * @param f mapping function
      */
    def captureMapR[R](
        missingHeaderResult: Option[F[BaseResult[F]]] = None,
        isRequired: Boolean = false)(
        f: HR => Either[F[BaseResult[F]], R]): TypedHeader[F, R :: HNil] =
      _captureMapR(
        missingHeaderResult.map(FailureResponse.result[F](_)),
        missingHeaderResult.isEmpty || isRequired
      )(f.andThen(ResultResponse.fromEither(_)))

    /** Capture a specific header and map its value
      *
      * @param f mapping function
      */
    def captureMapR[R](f: HR => Either[F[BaseResult[F]], R]): TypedHeader[F, R :: HNil] =
      captureMapR()(f)
  }

  private def _captureMapR[A, H[_], R](
      missingHeaderResult: Option[FailureResponse[F]],
      isRequired: Boolean)(f: H[A] => ResultResponse[F, R])(implicit
      F: Monad[F],
      H: Header[A, _],
      S: Header.Select.Aux[A, H]): TypedHeader[F, R :: HNil] =
    _captureMapR[A, H, R](isRequired) {
      case Some(header) => f(header)
      case None => missingHeaderResult.getOrElse(FailureResponse.result(missingHeaderResponse[A]))
    }

  private def _captureMapR[A, H[_], R](isRequired: Boolean)(
      f: Option[H[A]] => ResultResponse[F, R])(implicit
      F: Monad[F],
      H: Header[A, _],
      S: Header.Select.Aux[A, H]): TypedHeader[F, R :: HNil] =
    genericHeaderCapture[R] { headers =>
      def process(h: Option[H[A]]): ResultResponse[F, R] =
        try f(h)
        catch {
          case NonFatal(e) =>
            FailureResponse.result {
              errorProcessingHeaderResponse[A](h.map(S.toRaw), e)
            }
        }

      def errorParsingHeader(
          errors: NonEmptyList[ParseFailure]
      ) = FailureResponse.result(errorParsingHeaderResponse(errors))

      S.fromSafe(headers.headers) match {
        case None => process(Option.empty)
        case Some(Ior.Right(value)) => process(Option(value))
        case Some(Ior.Both(errors, _)) => errorParsingHeader(errors)
        case Some(Ior.Left(errors)) => errorParsingHeader(errors)
      }

    }.withMetadata(HeaderMetaData[A](H.name, isRequired))

  protected def invalidHeaderResponse[A](implicit F: Monad[F], H: Header[A, _]): F[BaseResult[F]] =
    BadRequest(s"Invalid header: ${H.name}").widen

  protected def missingHeaderResponse[A](implicit F: Monad[F], H: Header[A, _]): F[BaseResult[F]] =
    BadRequest(s"Missing header: ${H.name}").widen

  protected def errorParsingHeaderResponse[A, H[_]](
      errors: NonEmptyList[ParseFailure]
  )(implicit F: Monad[F], H: Header[A, _]): F[BaseResult[F]] =
    BadRequest(
      s"Failed to parse header: ${H.name} with ${errors.map(_.sanitized).mkString_(",")}"
    ).widen

  protected def errorProcessingHeaderResponse[A](
      header: Option[Header.Raw],
      nonfatal: Throwable)(implicit F: Monad[F], H: Header[A, _]): F[BaseResult[F]] = {
    logger.error(nonfatal) {
      val headerValue = header.fold(show""""${H.name}" was Undefined""")(_.show)
      s"""Failure during header capture: $headerValue"""
    }
    InternalServerError("Error processing request.").widen
  }
}
