package org.http4s
package rho

import cats.{Functor, Monad}
import cats.data.{Kleisli, OptionT}
import shapeless.{::, HNil}
import org.http4s.rho.bits.{FailureResponse, SuccessResponse, TypedHeader}


/** The [[AuthedContext]] provides a convenient way to define a RhoService
  * which works with http4s authentication middleware.
  * {{{
  *     case class User(name: String, id: UUID)
  *
  *     object Auth {
  *       val authUser: Service[Request, User] = Kleisli({ _ =>
  *         Task.now(User("Test User", UUID.randomUUID()))
  *       })
  *
  *       val authenticated = AuthMiddleware(authUser)
  *     }
  *
  *     object MyAuth extends AuthedContext[User]
  *
  *     object MyService extends RhoService {
  *       import MyAuth._
  *       GET +? param("foo", "bar") |>> { (req: Request, foo: String) =>
  *         val user = getAuth(req)
  *         if (user.name == "Test User") {
  *           Ok(s"just root with parameter 'foo=$foo'")
  *         } else {
  *           BadRequest("This should not have happened.")
  *         }
  *       }
  *     }
  *
  *     val service = Auth.authenticated(MyAuth.toService(MyService.toService(SwaggerSupport())))
  * }}}
  *
  * @tparam U authInfo type for this service.
  */
class AuthedContext[U] {

  /* Attribute key to lookup authInfo in request attributeMap . */
  final private val authKey = AttributeKey[U]

  /** Turn the [[HttpService]] into an `AuthedService`
    *
    * @param service [[HttpService]] to convert
    * @return An `AuthedService` which can be mounted by http4s servers.
    */
  def toService[F[_]: Functor](service: HttpService[F]): AuthedService[U, F] =
    Kleisli[OptionT[F, ?], AuthedRequest[F, U], Response[F]] { (a: AuthedRequest[F, U]) =>
      service(a.req.withAttribute[U](authKey, a.authInfo))
    }

  /* Get the authInfo object from request. */
  def getAuth[F[_]](req: Request[F]): U = {
    req.attributes.get[U](authKey).get
  }

  def auth[F[_]: Monad](): TypedHeader[F, U :: HNil] = rho.genericRequestHeaderCapture[F, U] { req =>
    req.attributes.get(authKey) match {
      case Some(authInfo) => SuccessResponse(authInfo)
      case None => FailureResponse.error[F, String]("Invalid auth configuration")
    }
  }
}
