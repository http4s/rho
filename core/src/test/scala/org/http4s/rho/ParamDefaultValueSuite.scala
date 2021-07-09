package org.http4s
package rho

import cats.effect.IO
import munit.CatsEffectSuite

import scala.collection.immutable.Seq

class ParamDefaultValueSuite extends CatsEffectSuite {
  private def body(routes: HttpRoutes[IO], r: Request[IO]): IO[String] =
    routes(r).value
      .flatMap(_.getOrElse(Response.notFound).body.compile.toVector)
      .map(_.foldLeft(Array[Byte]())(_ :+ _))
      .map(new String(_))

  private def requestGet(s: String, h: Header.ToRaw*): Request[IO] =
    Request(
      bits.MethodAliases.GET,
      Uri.fromString(s).getOrElse(sys.error("Failed.")),
      headers = Headers(h: _*)
    )

  val routes1 = new RhoRoutes[IO] {
    GET / "test1" +? param[String]("param1") |>> { param1: String => Ok("test1:" + param1) }
  }.toRoutes()

  test("GET /test1 should map parameter with default value") {
    assertIO(body(routes1, requestGet("/test1")), "Missing query param: param1")
  }

  test("GET /test1 should map parameter with empty value") {
    assertIO(body(routes1, requestGet("/test1?param1=")), "test1:")
  }

  test("GET /test1 should map parameter with value") {
    assertIO(body(routes1, requestGet("/test1?param1=value1")), "test1:value1")
  }

  test("GET /test1 should map parameter without value") {
    assertIO(
      body(
        routes1,
        requestGet("/test1?param1")
      ),
      "Value of query parameter 'param1' missing"
    )
  }

  val routes2 = new RhoRoutes[IO] {
    GET / "test2" +? param[String]("param1", "default1") |>> { param1: String =>
      Ok("test2:" + param1)
    }
  }.toRoutes()

  val default2 = "test2:default1"

  test("GET /test2 should map parameter with default value") {
    assertIO(body(routes2, requestGet("/test2")), default2)
  }

  test("GET /test2 should map parameter with empty value") {
    assertIO(body(routes2, requestGet("/test2?param1=")), "test2:")
  }

  test("GET /test2 should map parameter with value") {
    assertIO(body(routes2, requestGet("/test2?param1=value1")), "test2:value1")
  }

  test("GET /test2 should map parameter without value") {
    assertIO(body(routes2, requestGet("/test2?param1")), default2)
  }

  val routes3 = new RhoRoutes[IO] {
    GET / "test3" +? param[Int]("param1", 1) |>> { param1: Int => Ok("test3:" + param1) }
  }.toRoutes()

  val default3 = "test3:1"

  test("GET /test3 should map parameter with default value") {
    assertIO(body(routes3, requestGet("/test3")), default3)
  }

  test("GET /test3 should fail to map parameter with empty value") {
    assertIO(body(routes3, requestGet("/test3?param1=")), "Invalid number format: ''")
  }

  test("GET /test3 should map parameter with numeric value") {
    assertIO(body(routes3, requestGet("/test3?param1=12345")), "test3:12345")
  }

  test("GET /test3 should fail to map parameter with non-numeric value") {
    assertIO(
      body(
        routes3,
        requestGet("/test3?param1=value1")
      ),
      "Invalid number format: 'value1'"
    )
  }

  test("GET /test3 should map parameter without value") {
    assertIO(body(routes3, requestGet("/test3?param1")), default3)
  }

  val routes4 = new RhoRoutes[IO] {
    GET / "test4" +? param[Option[String]]("param1") |>> { os: Option[String] =>
      Ok("test4:" + os.getOrElse(""))
    }
  }.toRoutes()

  val default4 = "test4:"

  test("GET /test4 should map parameter with default value") {
    assertIO(body(routes4, requestGet("/test4")), default4)
  }

  test("GET /test4 should map parameter with empty value") {
    assertIO(body(routes4, requestGet("/test4?param1=")), "test4:")
  }

  test("GET /test4 should map parameter with value") {
    assertIO(body(routes4, requestGet("/test4?param1=value1")), "test4:value1")
  }

  test("GET /test4 should map parameter without value") {
    assertIO(body(routes4, requestGet("/test4?param1")), "test4:")
  }

  val routes5 = new RhoRoutes[IO] {
    GET / "test5" +? param[Option[Int]]("param1", Some(100)) |>> { os: Option[Int] =>
      Ok("test5:" + os.getOrElse(""))
    }
  }.toRoutes()

  val default5 = "test5:100"

  test("GET /test5 should map parameter with default value") {
    assertIO(body(routes5, requestGet("/test5")), default5)
  }

  test("GET /test5 should fail on parameter with empty value") {
    assertIO(body(routes5, requestGet("/test5?param1=")), "Invalid number format: ''")
  }

  test("GET /test5 should map parameter with numeric value") {
    assertIO(body(routes5, requestGet("/test5?param1=12345")), "test5:12345")
  }

  test("GET /test5 should fail on parameter with non-numeric value") {
    assertIO(
      body(
        routes5,
        requestGet("/test5?param1=value1")
      ),
      "Invalid number format: 'value1'"
    )
  }

  test("GET /test5 should map parameter without value") {
    assertIO(body(routes5, requestGet("/test5?param1")), default5)
  }

  val routes6 = new RhoRoutes[IO] {
    GET / "test6" +? param[Option[String]]("param1", Some("default1")) |>> { os: Option[String] =>
      Ok("test6:" + os.getOrElse(""))
    }
  }.toRoutes()

  val default6 = "test6:default1"

  test("GET /test6 should map parameter with default value") {
    assertIO(body(routes6, requestGet("/test6")), default6)
  }

  test("GET /test6 should map parameter with empty value") {
    assertIO(body(routes6, requestGet("/test6?param1=")), "test6:")
  }

  test("GET /test6 should map parameter with value") {
    assertIO(body(routes6, requestGet("/test6?param1=test12345")), "test6:test12345")
  }

  test("GET /test6 should map parameter without value") {
    assertIO(body(routes6, requestGet("/test6?param1")), default6)
  }

  val routes7 = new RhoRoutes[IO] {
    GET / "test7" +? param[Seq[String]]("param1", Seq("a", "b")) |>> { os: Seq[String] =>
      Ok("test7:" + os.mkString(","))
    }
  }.toRoutes()

  val default7 = "test7:a,b"

  test("GET /test7 should map parameter with default value") {
    assertIO(body(routes7, requestGet("/test7")), default7)
  }

  test("GET /test7 should map parameter with empty value") {
    assertIO(body(routes7, requestGet("/test7?param1=")), "test7:")
  }

  test("GET /test7 should map parameter with one value") {
    assertIO(body(routes7, requestGet("/test7?param1=test12345")), "test7:test12345")
  }

  test("GET /test7 should map parameter with many values") {
    assertIO(
      body(
        routes7,
        requestGet("/test7?param1=test123&param1=test456&param1=test889")
      ),
      "test7:test123,test456,test889"
    )
  }

  test("GET /test7 should map parameter without value") {
    assertIO(body(routes7, requestGet("/test7?param1")), default7)
  }

  val routes8 = new RhoRoutes[IO] {
    GET / "test8" +? param[Seq[Int]]("param1", Seq(3, 5, 8)) |>> { os: Seq[Int] =>
      Ok("test8:" + os.mkString(","))
    }
  }.toRoutes()

  val default8 = "test8:3,5,8"

  test("GET /test8 should map parameter with default value") {
    assertIO(body(routes8, requestGet("/test8")), default8)
  }

  test("GET /test8 should fail to map parameter with empty value") {
    assertIO(body(routes8, requestGet("/test8?param1=")), "Invalid number format: ''")
  }

  test("GET /test8 should map parameter with one numeric value") {
    assertIO(body(routes8, requestGet("/test8?param1=12345")), "test8:12345")
  }

  test("GET /test8 should fail to map parameter with one non-numeric value") {
    assertIO(body(routes8, requestGet("/test8?param1=test")), "Invalid number format: 'test'")
  }

  test("GET /test8 should map parameter with many numeric values") {
    assertIO(
      body(
        routes8,
        requestGet("/test8?param1=123&param1=456&param1=789")
      ),
      "test8:123,456,789"
    )
  }

  test("GET /test8 should fail to map parameter with many non-numeric values") {
    assertIO(
      body(
        routes8,
        requestGet("/test8?param1=abc&param1=def")
      ),
      "Invalid number format: 'abc'"
    )
  }

  test("GET /test8 should map parameter without value") {
    assertIO(body(routes8, requestGet("/test8?param1")), default8)
  }

  val routes9 = new RhoRoutes[IO] {
    GET / "test9" +? param("param1", "default1", (p: String) => !p.isEmpty && p != "fail") |>> {
      param1: String => Ok("test9:" + param1)
    }
  }.toRoutes()

  val default9 = "test9:default1"

  test("GET /test9 should map parameter with default value") {
    assertIO(body(routes9, requestGet("/test9")), default9)
  }

  test("GET /test9 should fail to map parameter with empty value") {
    assertIO(
      body(
        routes9,
        requestGet("/test9?param1=")
      ),
      "Invalid query parameter: \"param1\" = \"\""
    )
  }

  test("GET /test9 should fail to map parameter with invalid value") {
    assertIO(
      body(
        routes9,
        requestGet("/test9?param1=fail")
      ),
      "Invalid query parameter: \"param1\" = \"fail\""
    )
  }

  test("GET /test9 should map parameter with valid value") {
    assertIO(body(routes9, requestGet("/test9?param1=pass")), "test9:pass")
  }

  test("GET /test9 should map parameter without value") {
    assertIO(body(routes9, requestGet("/test9?param1")), default9)
  }

  val routes10 = new RhoRoutes[IO] {
    GET / "test10" +? param[Int]("param1", 1, (p: Int) => p >= 0) |>> { param1: Int =>
      Ok("test10:" + param1)
    }
  }.toRoutes()

  val default10 = "test10:1"

  test("GET /test10 should map parameter with default value") {
    assertIO(body(routes10, requestGet("/test10")), default10)
  }

  test("GET /test10 should fail to map parameter with empty value") {
    assertIO(body(routes10, requestGet("/test10?param1=")), "Invalid number format: ''")
  }

  test("GET /test10 should fail to map parameter with invalid numeric value") {
    assertIO(
      body(
        routes10,
        requestGet("/test10?param1=-4")
      ),
      "Invalid query parameter: \"param1\" = \"-4\""
    )
  }

  test("GET /test10 should fail to map parameter with non-numeric value") {
    assertIO(
      body(
        routes10,
        requestGet("/test10?param1=value1")
      ),
      "Invalid number format: 'value1'"
    )
  }

  test("GET /test10 should map parameter with valid numeric value") {
    assertIO(body(routes10, requestGet("/test10?param1=10")), "test10:10")
  }

  test("GET /test10 should map parameter without value") {
    assertIO(body(routes10, requestGet("/test10?param1")), default10)
  }

  val routes11 = new RhoRoutes[IO] {
    GET / "test11" +? param[Option[Int]](
      "param1",
      Some(100),
      (p: Option[Int]) => p != Some(0)
    ) |>> { os: Option[Int] =>
      Ok("test11:" + os.getOrElse(""))
    }
  }.toRoutes()

  val default11 = "test11:100"

  test("GET /test11 should map parameter with default value") {
    assertIO(body(routes11, requestGet("/test11")), default11)
  }

  test("GET /test11 should fail to map parameter with empty value") {
    assertIO(body(routes11, requestGet("/test11?param1=")), "Invalid number format: ''")
  }

  test("GET /test11 should fail to map parameter with invalid numeric value") {
    assertIO(
      body(
        routes11,
        requestGet("/test11?param1=0")
      ),
      "Invalid query parameter: \"param1\" = \"Some(0)\""
    )
  }

  test("GET /test11 should fail to map parameter with non-numeric value") {
    assertIO(
      body(
        routes11,
        requestGet("/test11?param1=value1")
      ),
      "Invalid number format: 'value1'"
    )
  }

  test("GET /test11 should map parameter with valid numeric value") {
    assertIO(body(routes11, requestGet("/test11?param1=1")), "test11:1")
  }

  test("GET /test11 should map parameter without value") {
    assertIO(body(routes11, requestGet("/test11?param1")), default11)
  }

  val routes12 = new RhoRoutes[IO] {
    GET / "test12" +? param[Option[String]](
      "param1",
      Some("default1"),
      (p: Option[String]) => p != Some("fail") && p != Some("")
    ) |>> { os: Option[String] =>
      Ok("test12:" + os.getOrElse(""))
    }
  }.toRoutes()

  val default12 = "test12:default1"

  test("GET /test12 should map parameter with default value") {
    assertIO(body(routes12, requestGet("/test12")), default12)
  }

  test("GET /test12 should fail to map parameter with empty value") {
    assertIO(
      body(
        routes12,
        requestGet("/test12?param1=")
      ),
      "Invalid query parameter: \"param1\" = \"Some()\""
    )
  }

  test("GET /test12 should fail to map parameter with invalid value") {
    assertIO(
      body(
        routes12,
        requestGet("/test12?param1=fail")
      ),
      "Invalid query parameter: \"param1\" = \"Some(fail)\""
    )
  }

  test("GET /test12 should map parameter with valid value") {
    assertIO(body(routes12, requestGet("/test12?param1=pass")), "test12:pass")
  }

  test("GET /test12 should map parameter without value") {
    assertIO(body(routes12, requestGet("/test12?param1")), default12)
  }

  val routes13 = new RhoRoutes[IO] {
    GET / "test13" +? param[Seq[String]](
      "param1",
      Seq("a", "b"),
      (p: Seq[String]) => !p.contains("") && !p.contains("z")
    ) |>> { os: Seq[String] =>
      Ok("test13:" + os.mkString(","))
    }
  }.toRoutes()

  val default13 = "test13:a,b"

  test("GET /test13 should map parameter with default value") {
    assertIO(body(routes13, requestGet("/test13")), default13)
  }

  test("GET /test13 should fail to map parameter with empty value") {
    assertIO_(
      body(routes13, requestGet("/test13?param1=")).map(x =>
        assert(
          x.matches(
            """Invalid query parameter: "param1" = "(List|Vector)\(\)""""
          )
        )
      )
    )
  }

  test("GET /test13 should fail to map parameter with one invalid value") {
    assertIO_(
      body(routes13, requestGet("/test13?param1=z")).map(x =>
        assert(
          x.matches(
            """Invalid query parameter: "param1" = "(List|Vector)\(z\)""""
          )
        )
      )
    )
  }

  test("GET /test13 should map parameter with many values and one invalid") {
    assertIO_(
      body(routes13, requestGet("/test13?param1=z&param1=aa&param1=bb")).map(x =>
        assert(
          x.matches(
            """Invalid query parameter: "param1" = "(List|Vector)\(z, aa, bb\)""""
          )
        )
      )
    )
  }

  test("GET /test13 should map parameter with many valid values") {
    assertIO(body(routes13, requestGet("/test13?param1=c&param1=d")), "test13:c,d")
  }

  test("GET /test13 should map parameter without value") {
    assertIO(body(routes13, requestGet("/test13?param1")), default13)
  }

  val routes14 = new RhoRoutes[IO] {
    GET / "test14" +? param[Seq[Int]](
      "param1",
      Seq(3, 5, 8),
      (p: Seq[Int]) => p != Seq(8, 5, 3)
    ) |>> { os: Seq[Int] => Ok("test14:" + os.mkString(",")) }
  }.toRoutes()

  val default14 = "test14:3,5,8"

  test("GET /test14 should map parameter with default value") {
    assertIO(body(routes14, requestGet("/test14")), default14)
  }

  test("GET /test14 should fail to map parameter with empty value") {
    assertIO(body(routes14, requestGet("/test14?param1=")), "Invalid number format: ''")
  }

  test("GET /test14 should fail to map parameter with one invalid numeric value") {
    assertIO_(
      body(routes14, requestGet("/test14?param1=8&param1=5&param1=3")).map(x =>
        assert(
          x.matches(
            """Invalid query parameter: "param1" = "(List|Vector)\(8, 5, 3\)""""
          )
        )
      )
    )
  }

  test("GET /test14 should fail to map parameter with one non-numeric value") {
    assertIO(
      body(
        routes14,
        requestGet("/test14?param1=test")
      ),
      "Invalid number format: 'test'"
    )
  }

  test("GET /test14 should fail to map parameter with many non-numeric values") {
    assertIO(
      body(
        routes14,
        requestGet("/test14?param1=abc&param1=def")
      ),
      "Invalid number format: 'abc'"
    )
  }

  test("GET /test14 should map parameter with many valid numeric values") {
    assertIO(body(routes14, requestGet("/test14?param1=1&param1=2&param1=3")), "test14:1,2,3")
  }

  test("GET /test14 should map parameter without value") {
    assertIO(body(routes14, requestGet("/test14?param1")), default14)
  }
}
