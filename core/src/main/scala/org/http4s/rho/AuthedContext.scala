package org.http4s
package rho

import cats.Monad
import cats.data.{Kleisli, OptionT}
import shapeless.{::, HNil}
import org.http4s.rho.bits.{FailureResponseOps, SuccessResponse, TypedHeader}
import _root_.io.chrisdavenport.vault._
import cats.effect._

/** The [[AuthedContext]] provides a convenient way to define a RhoRoutes
  * which works with http4s authentication middleware.
  * Please note that `AuthMiddleware`-wrapping is mandatory, otherwise context
  * doesn't take effect.
  * {{{
  *     case class User(name: String, id: UUID)
  *
  *     val authUser: Kleisli[IO, Request[IO], Either[String, User]] = Kleisli{ req =>
  *       IO(Right(User("Bob", UUID.randomUUID())))
  *     }
  *
  *     val onFailure: AuthedRoutes[String, IO] = Kleisli(req => OptionT.liftF(Forbidden(req.authInfo)))
  *
  *     val middleware = AuthMiddleware(authUser, onFailure)
  *
  *     object Auth extends AuthedContext[IO, User]
  *
  *     object BobRoutes extends RhoRoutes[IO] {
  *       GET +? param("foo", "bar") >>> Auth.auth |>> { (foo: String, user: User) =>
  *         Ok(s"Bob with id \${user.id}, foo \$foo")
  *       }
  *     }
  *
  *     val service = middleware.apply(Auth.toService(BobRoutes.toRoutes()))
  * }}}
  *
  * @tparam U authInfo type for this service.
  */
class AuthedContext[F[_]: Monad, U] extends FailureResponseOps[F] {

  /* Attribute key to lookup authInfo in request attributeMap . */
  final private val authKey = Key.newKey[SyncIO, U].unsafeRunSync()

  /** Turn the [[HttpRoutes]] into an `AuthedService`
    *
    * @param routes [[HttpRoutes]] to convert
    * @return An `AuthedService` which can be mounted by http4s servers.
    */
  def toService(routes: HttpRoutes[F]): AuthedRoutes[U, F] = {
    type O[A] = OptionT[F, A]

    Kleisli[O, AuthedRequest[F, U], Response[F]] { a: AuthedRequest[F, U] =>
      routes(a.req.withAttribute[U](authKey, a.context))
    }
  }

  /** Get the authInfo object from request if `AuthMiddleware` provided one */
  def getAuth(req: Request[F]): Option[U] =
    req.attributes.lookup(authKey)

  /** Request matcher to capture authentication information */
  def auth: TypedHeader[F, U :: HNil] = RhoDsl[F].genericRequestHeaderCapture[U] { req =>
    getAuth(req) match {
      case Some(authInfo) => SuccessResponse(authInfo)
      case None => error("Invalid auth configuration")
    }
  }
}
