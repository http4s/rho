import org.http4s.rho.{RhoService, _}
import org.http4s.rho.bits.MethodAliases._
import java.util.concurrent.atomic.AtomicInteger
import java.time.Instant

import org.http4s.{EntityEncoder, Method, Uri}
import org.http4s.Method.NoBody
import org.http4s.Method.Semantics.Safe
import org.http4s.rho.RhoRoute.Tpe
import org.http4s.rho.bits.ResultMatcher.MaybeWritable
import org.http4s.rho.bits.{HListToFunc, ResultMatcher}
import shapeless.HNil

import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

object route extends RhoService {

}

