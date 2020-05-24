import org.http4s.rho._
import org.specs2.mutable.Specification
import cats.data._
import cats.effect.IO
import org.http4s._
import org.http4s.server._
import org.http4s.util.CaseInsensitiveString


class AuthedApiExamples extends Specification {
  case class SimpleUser(name: String)

  val simpleAuthService: Kleisli[OptionT[IO, ?], Request[IO], SimpleUser] =
    Kleisli { request =>
      OptionT.fromOption[IO] {
        request.headers.get(CaseInsensitiveString("user")).map(h => SimpleUser(h.value))
      }
    }
  val simpleAuthMiddlware: AuthMiddleware[IO, SimpleUser] = AuthMiddleware(simpleAuthService)

  "mock api" should {
    "Make it easy to create authed routes" in {

      val authed = new AuthedRhoRoutes[IO, SimpleUser] {
        GET / "hello" |>> { () =>
          Kleisli.ask[IO, SimpleUser].flatMap {
            user: SimpleUser =>
            Ok(s"Hello, ${user.name}!")
          }
        }
      }

      val authedHttpService: AuthedRoutes[SimpleUser, IO] = AuthedRhoRoutesOps.toAuthedService(authed)

      simpleAuthMiddlware(authedHttpService)

      ok
    }
  }
}
