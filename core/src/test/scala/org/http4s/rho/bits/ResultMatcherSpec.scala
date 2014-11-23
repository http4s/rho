package org.http4s
package rho
package bits

import org.specs2.mutable.Specification
import shapeless.HList

import Status._

class ResultMatcherSpec extends Specification {

  trait TRhoService extends RhoService {
    var statuses: Set[Status] = Set.empty

    override protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
      statuses = action.resultInfo.collect {
        case StatusOnly(s)        => s
        case StatusAndModel(s, _) => s
      }
      super.append(action)
    }
  }

  "ResponseGenerator" should {
    "Match a single result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () => Ok("updated").run }
      }

      srvc.statuses should_== Set(Ok)
    }

    "Match two results with different status with different result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () =>
          val a = 0
          a match {
            case 0 => NotFound(s"Not found")
            case 1 => Ok(<html><body>Hello world</body></html>)
          }
        }
      }

      srvc.statuses should_== Set(NotFound, Ok)
    }

    "Match an empty result type" in {
      val srvc = new TRhoService {
        PUT / "foo" |>> { () => NoContent() }
      }

      srvc.statuses should_== Set(NoContent)
    }
//
//    "Match three results with different status but same result type" in {
//      val srvc = new TRhoService {
//        PUT / "foo" |>> { () =>
//          val a = 0
//          a match {
//            case 0 => NotFound(s"Not found")
//            case 1 => Ok("updated")
//            //case 2 => Accepted("it was accepted")
//          }
//        }
//      }
//
//      srvc.statuses should_== Set(NotFound, Ok, Accepted)
//    }
//
//    "Match four results with different status but same result type" in {
//      val srvc = new TRhoService {
//        PUT / "foo" |>> { () =>
//          val a = 0
//          a match {
//            case 0 => NotFound(s"Not found")
//            case 1 => Ok("updated")
//            //case 2 => Accepted("it was accepted")
//            //case 4 => Created("it was created")
//          }
//        }
//      }
//
//      srvc.statuses should_== Set(NotFound, Ok, Accepted, Created)
//    }
//
//        "Match two results with same status but different result type" in {
    //      val srvc = new RhoService {
    //        PUT / "foo" |>> { () =>
    //          val a = 0
    //          a match {
    //            case 0 => Ok(<html><body>foo</body></html>)
    //            case 1 => Ok("updated")
    //          }
    //        }
    //      }
    //
    //      true should_== true
    //    }
  }
}