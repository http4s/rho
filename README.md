ρ: A DSL for building HTTP services with http4s
-----------------------------------------------

[![CI](https://github.com/http4s/rho/workflows/CI/badge.svg)](https://github.com/http4s/rho/actions?query=workflow%3A%22CI%22)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.http4s/rho-core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.http4s/rho-core_2.12)
[![Gitter](https://badges.gitter.im/http4s/rho.svg)](https://gitter.im/http4s/rho?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)


```scala
val httpService = new RhoRoutes[IO] {
   GET / "hello" / pv"world" +? param[Int]("fav") |>> { (world: String, fav: Int) => 
     Ok(s"Received $fav, $world") 
   }
}
```

See the [tutorial](https://github.com/http4s/rho/blob/master/Rho.md), [wiki](https://github.com/http4s/rho/wiki) and the [tests](https://github.com/http4s/rho/blob/master/core/src/test/scala/ApiExamples.scala) for more examples.

### Get more from your route definitions
The primary goal of ρ is to provide an easy to use AST with which to build HTTP services which can be inspected to extract a variety of information including:
* [Swagger documentation](http://swagger.io/)
* HAL documentation

Get ρ
-----
Rho artifacts are available at Maven Central and snapshots are available from the Sonatype repositories.

Read the [Rho Scaladocs](https://www.javadoc.io/doc/org.http4s/rho-core_2.13/latest/index.html)

```scala
resolvers += Resolver.sonatypeRepo("snapshots")  // Only if you are using a -snapshot version

libraryDependencies += "org.http4s" %% "rho-swagger" % version

```

Stability
---------
ρ remains a work in progress. However, it is now approaching a point where things are
beginning to stabilize. Going forward changes to the api should will come more slowly 
and have deprecation period.

Contributing
------------
Contributions of all kinds are welcome! Documentation contributions are especially useful 
for others who are also just learning to use ρ. The wiki and the tests are the primary
source of documentation. Pull requests are greatly appreciated from their original authors,
licensed to the http4s project under the project's
[open source license](https://github.com/http4s/rho/blob/master/LICENSE).

License
-------
ρ is licensed under the terms of the Apache 2.0 license. See the license file in the base
directory for more information.
