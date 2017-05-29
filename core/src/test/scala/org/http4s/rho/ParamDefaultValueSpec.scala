package org.http4s
package rho

import org.http4s.HttpService
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class ParamDefaultValueSpec extends Specification {

  def body(service: HttpService, r: Request): String =
    new String(service(r).unsafeRun.orNotFound.body.runLog.unsafeRun.foldLeft(ByteVector.empty)(_ :+ _).toArray)

  def requestGet(s: String, h: Header*): Request =
    Request(bits.MethodAliases.GET, Uri.fromString(s).right.getOrElse(sys.error("Failed.")), headers = Headers(h: _*))

  "GET /test1" should {
    val service = new RhoService {
      GET / "test1" +? param[String]("param1") |>> { param1: String => Ok("test1:" + param1) }
    }.toService()

    val default = "test1:default1"
    "map parameter with default value" in {
      body(service, requestGet("/test1")) must be equalTo "Missing query param: param1"
    }
    "map parameter with empty value" in {
      body(service, requestGet("/test1?param1=")) must be equalTo "test1:"
    }
    "map parameter with value" in {
      body(service, requestGet("/test1?param1=value1")) must be equalTo "test1:value1"
    }
    "map parameter without value" in {
      body(service, requestGet("/test1?param1")) must be equalTo "Value of query parameter 'param1' missing"
    }
  }

  "GET /test2" should {
    val service = new RhoService {
      GET / "test2" +? param("param1", "default1") |>> { param1: String => Ok("test2:" + param1) }
    }.toService()

    val default = "test2:default1"
    "map parameter with default value" in {
      body(service, requestGet("/test2")) must be equalTo default
    }
    "map parameter with empty value" in {
      body(service, requestGet("/test2?param1=")) must be equalTo "test2:"
    }
    "map parameter with value" in {
      body(service, requestGet("/test2?param1=value1")) must be equalTo "test2:value1"
    }
    "map parameter without value" in {
      body(service, requestGet("/test2?param1")) must be equalTo default
    }
  }

  "GET /test3" should {
    val service = new RhoService {
      GET / "test3" +? param[Int]("param1", 1) |>> { param1: Int => Ok("test3:" + param1) }
    }.toService()

    val default = "test3:1"
    "map parameter with default value" in {
      body(service, requestGet("/test3")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test3?param1=")) must be equalTo "Invalid number format: ''"
    }
    "map parameter with numeric value" in {
      body(service, requestGet("/test3?param1=12345")) must be equalTo "test3:12345"
    }
    "fail to map parameter with non-numeric value" in {
      body(service, requestGet("/test3?param1=value1")) must be equalTo "Invalid number format: 'value1'"
    }
    "map parameter without value" in {
      body(service, requestGet("/test3?param1")) must be equalTo default
    }
  }

  "GET /test4" should {
    val service = new RhoService {
      GET / "test4" +? param[Option[String]]("param1") |>> { os: Option[String] => Ok("test4:" + os.getOrElse("")) }
    }.toService()

    val default = "test4:"
    "map parameter with default value" in {
      body(service, requestGet("/test4")) must be equalTo default
    }
    "map parameter with empty value" in {
      body(service, requestGet("/test4?param1=")) must be equalTo "test4:"
    }
    "map parameter with value" in {
      body(service, requestGet("/test4?param1=value1")) must be equalTo "test4:value1"
    }
    "map parameter without value" in {
      body(service, requestGet("/test4?param1")) must be equalTo "test4:"
    }
  }

  "GET /test5" should {
    val service = new RhoService {
      GET / "test5" +? param[Option[Int]]("param1", Some(100)) |>> { os: Option[Int] => Ok("test5:" + os.getOrElse("")) }
    }.toService()

    val default = "test5:100"
    "map parameter with default value" in {
      body(service, requestGet("/test5")) must be equalTo default
    }
    "fail on parameter with empty value" in {
      body(service, requestGet("/test5?param1=")) must be equalTo "Invalid number format: ''"
    }
    "map parameter with numeric value" in {
      body(service, requestGet("/test5?param1=12345")) must be equalTo "test5:12345"
    }
    "fail on parameter with non-numeric value" in {
      body(service, requestGet("/test5?param1=value1")) must be equalTo "Invalid number format: 'value1'"
    }
    "map parameter without value" in {
      body(service, requestGet("/test5?param1")) must be equalTo default
    }
  }

  "GET /test6" should {
    val service = new RhoService {
      GET / "test6" +? param[Option[String]]("param1", Some("default1")) |>> { os: Option[String] => Ok("test6:" + os.getOrElse("")) }
    }.toService()

    val default = "test6:default1"
    "map parameter with default value" in {
      body(service, requestGet("/test6")) must be equalTo default
    }
    "map parameter with empty value" in {
      body(service, requestGet("/test6?param1=")) must be equalTo "test6:"
    }
    "map parameter with value" in {
      body(service, requestGet("/test6?param1=test12345")) must be equalTo "test6:test12345"
    }
    "map parameter without value" in {
      body(service, requestGet("/test6?param1")) must be equalTo default
    }
  }

  "GET /test7" should {
    val service = new RhoService {
      GET / "test7" +? param[Seq[String]]("param1", Seq("a", "b")) |>> { os: Seq[String] => Ok("test7:" + os.mkString(",")) }
    }.toService()

    val default = "test7:a,b"
    "map parameter with default value" in {
      body(service, requestGet("/test7")) must be equalTo default
    }
    "map parameter with empty value" in {
      body(service, requestGet("/test7?param1=")) must be equalTo "test7:"
    }
    "map parameter with one value" in {
      body(service, requestGet("/test7?param1=test12345")) must be equalTo "test7:test12345"
    }
    "map parameter with many values" in {
      body(service, requestGet("/test7?param1=test123&param1=test456&param1=test889")) must be equalTo "test7:test123,test456,test889"
    }
    "map parameter without value" in {
      body(service, requestGet("/test7?param1")) must be equalTo default
    }
  }

  "GET /test8" should {
    val service = new RhoService {
      GET / "test8" +? param[Seq[Int]]("param1", Seq(3, 5, 8)) |>> { os: Seq[Int] => Ok("test8:" + os.mkString(",")) }
    }.toService()

    val default = "test8:3,5,8"
    "map parameter with default value" in {
      body(service, requestGet("/test8")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test8?param1=")) must be equalTo "Invalid number format: ''"
    }
    "map parameter with one numeric value" in {
      body(service, requestGet("/test8?param1=12345")) must be equalTo "test8:12345"
    }
    "fail to map parameter with one non-numeric value" in {
      body(service, requestGet("/test8?param1=test")) must be equalTo "Invalid number format: 'test'"
    }
    "map parameter with many numeric values" in {
      body(service, requestGet("/test8?param1=123&param1=456&param1=789")) must be equalTo "test8:123,456,789"
    }
    "fail to map parameter with many non-numeric values" in {
      body(service, requestGet("/test8?param1=abc&param1=def")) must be equalTo "Invalid number format: 'abc'"
    }
    "map parameter without value" in {
      body(service, requestGet("/test8?param1")) must be equalTo default
    }
  }

  "GET /test9" should {
    val service = new RhoService {
      GET / "test9" +? param("param1", "default1", (p: String) => !p.isEmpty && p != "fail") |>> { param1: String => Ok("test9:" + param1) }
    }.toService()

    val default = "test9:default1"
    "map parameter with default value" in {
      body(service, requestGet("/test9")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test9?param1=")) must be equalTo "Invalid query parameter: \"param1\" = \"\""
    }
    "fail to map parameter with invalid value" in {
      body(service, requestGet("/test9?param1=fail")) must be equalTo "Invalid query parameter: \"param1\" = \"fail\""
    }
    "map parameter with valid value" in {
      body(service, requestGet("/test9?param1=pass")) must be equalTo "test9:pass"
    }
    "map parameter without value" in {
      body(service, requestGet("/test9?param1")) must be equalTo default
    }
  }

  "GET /test10" should {
    val service = new RhoService {
      GET / "test10" +? param[Int]("param1", 1, (p: Int) => p >= 0) |>> { param1: Int => Ok("test10:" + param1) }
    }.toService()

    val default = "test10:1"
    "map parameter with default value" in {
      body(service, requestGet("/test10")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test10?param1=")) must be equalTo "Invalid number format: ''"
    }
    "fail to map parameter with invalid numeric value" in {
      body(service, requestGet("/test10?param1=-4")) must be equalTo "Invalid query parameter: \"param1\" = \"-4\""
    }
    "fail to map parameter with non-numeric value" in {
      body(service, requestGet("/test10?param1=value1")) must be equalTo "Invalid number format: 'value1'"
    }
    "map parameter with valid numeric value" in {
      body(service, requestGet("/test10?param1=10")) must be equalTo "test10:10"
    }
    "map parameter without value" in {
      body(service, requestGet("/test10?param1")) must be equalTo default
    }
  }

  "GET /test11" should {
    val service = new RhoService {
      GET / "test11" +? param[Option[Int]]("param1", Some(100), (p: Option[Int]) => p != Some(0)) |>> { os: Option[Int] => Ok("test11:" + os.getOrElse("")) }
    }.toService()

    val default = "test11:100"
    "map parameter with default value" in {
      body(service, requestGet("/test11")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test11?param1=")) must be equalTo "Invalid number format: ''"
    }
    "fail to map parameter with invalid numeric value" in {
      body(service, requestGet("/test11?param1=0")) must be equalTo "Invalid query parameter: \"param1\" = \"Some(0)\""
    }
    "fail to map parameter with non-numeric value" in {
      body(service, requestGet("/test11?param1=value1")) must be equalTo "Invalid number format: 'value1'"
    }
    "map parameter with valid numeric value" in {
      body(service, requestGet("/test11?param1=1")) must be equalTo "test11:1"
    }
    "map parameter without value" in {
      body(service, requestGet("/test11?param1")) must be equalTo default
    }
  }

  "GET /test12" should {
    val service = new RhoService {
      GET / "test12" +? param[Option[String]]("param1", Some("default1"), (p: Option[String]) => p != Some("fail") && p != Some("")) |>> { os: Option[String] => Ok("test12:" + os.getOrElse("")) }
    }.toService()

    val default = "test12:default1"
    "map parameter with default value" in {
      body(service, requestGet("/test12")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test12?param1=")) must be equalTo "Invalid query parameter: \"param1\" = \"Some()\""
    }
    "fail to map parameter with invalid value" in {
      body(service, requestGet("/test12?param1=fail")) must be equalTo "Invalid query parameter: \"param1\" = \"Some(fail)\""
    }
    "map parameter with valid value" in {
      body(service, requestGet("/test12?param1=pass")) must be equalTo "test12:pass"
    }
    "map parameter without value" in {
      body(service, requestGet("/test12?param1")) must be equalTo default
    }
  }

  "GET /test13" should {
    val service = new RhoService {
      GET / "test13" +? param[Seq[String]]("param1", Seq("a", "b"), (p: Seq[String]) => !p.contains("") && !p.contains("z")) |>> { os: Seq[String] => Ok("test13:" + os.mkString(",")) }
    }.toService()

    val default = "test13:a,b"
    "map parameter with default value" in {
      body(service, requestGet("/test13")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test13?param1=")) must be equalTo "Invalid query parameter: \"param1\" = \"List()\""
    }
    "fail to map parameter with one invalid value" in {
      body(service, requestGet("/test13?param1=z")) must be equalTo "Invalid query parameter: \"param1\" = \"List(z)\""
    }
    "map parameter with many values and one invalid" in {
      body(service, requestGet("/test13?param1=z&param1=aa&param1=bb")) must be equalTo "Invalid query parameter: \"param1\" = \"List(z, aa, bb)\""
    }
    "map parameter with many valid values" in {
      body(service, requestGet("/test13?param1=c&param1=d")) must be equalTo "test13:c,d"
    }
    "map parameter without value" in {
      body(service, requestGet("/test13?param1")) must be equalTo default
    }
  }

  "GET /test14" should {

    val service = new RhoService {
      GET / "test14" +? param[Seq[Int]]("param1", Seq(3, 5, 8), (p: Seq[Int]) => p != Seq(8, 5, 3)) |>> { os: Seq[Int] => Ok("test14:" + os.mkString(",")) }
    }.toService()

    val default = "test14:3,5,8"
    "map parameter with default value" in {
      body(service, requestGet("/test14")) must be equalTo default
    }
    "fail to map parameter with empty value" in {
      body(service, requestGet("/test14?param1=")) must be equalTo "Invalid number format: ''"
    }
    "fail to map parameter with one invalid numeric value" in {
      body(service, requestGet("/test14?param1=8&param1=5&param1=3")) must be equalTo "Invalid query parameter: \"param1\" = \"List(8, 5, 3)\""
    }
    "fail to map parameter with one non-numeric value" in {
      body(service, requestGet("/test14?param1=test")) must be equalTo "Invalid number format: 'test'"
    }
    "fail to map parameter with many non-numeric values" in {
      body(service, requestGet("/test14?param1=abc&param1=def")) must be equalTo "Invalid number format: 'abc'"
    }
    "map parameter with many valid numeric values" in {
      body(service, requestGet("/test14?param1=1&param1=2&param1=3")) must be equalTo "test14:1,2,3"
    }
    "map parameter without value" in {
      body(service, requestGet("/test14?param1")) must be equalTo default
    }
  }
}
