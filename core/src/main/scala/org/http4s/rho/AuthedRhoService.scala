package org.http4s
package rho


/** The [[AuthedRhoService]] provides a convenient way to define a RhoService
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
  *     object MyAuth extends AuthedRhoService[User]
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
  *     val service = Auth.authenticated(MyAuth.toService(MyService))
  * }}}
  *
  * @tparam U authInfo type for this service.
  */
class AuthedRhoService[U] {

  /* Attribute key to lookup authInfo in request attributeMap . */
  final private val authKey = AttributeKey[U]("authInfo")

  /** Turn the [[HttpService]] into an `AuthedService`
    *
    * @param rhoService [[RhoService]] to convert
    * @return An `AuthedService` which can be mounted by http4s servers.
    */
  def toService(rhoService: RhoService): AuthedService[U] = {
    val service = rhoService.toService()
    Service.lift { case AuthedRequest(authInfo, req) =>
      service(req.withAttribute[U](authKey, authInfo))
    }
  }

  /* Get the authInfo object from request. */
  def getAuth(req: Request): U = {
    req.attributes.get[U](authKey).get
  }
}

