ρ: A DSL for building HTTP services with http4s
-----------------------------------------------

[![Build Status](https://travis-ci.org/http4s/rho.svg?branch=master)](https://travis-ci.org/http4s/rho)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.http4s/rho-core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.http4s/rho-core_2.12)
[![Gitter](https://badges.gitter.im/http4s/rho.svg)](https://gitter.im/http4s/rho?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

```scala
val httpService = new RhoService[IO] {
   GET / "hello" / 'world +? param[Int]("fav") |>> { (world: String, fav: Int) => 
     Ok(s"Received $fav, $world") 
   }
}
```

See the [tutorial](https://github.com/http4s/rho/blob/master/Rho.md), [wiki](https://github.com/http4s/rho/wiki) and the [tests](https://github.com/http4s/rho/blob/master/core/src/test/scala/ApiExamples.scala) for more examples.

### Get more from your route definitions
The primary goal of ρ is to provide an easy to use AST with which to build HTTP services which can be inspected to extract a variety of information including:
* [Swagger documentation](http://swagger.wordnik.com/)
* HAL documentation

Get ρ
-----
Rho artifacts are available at Maven Central and snapshots are available from the Sonatype repositories.

Read the [Rho Scaladocs](http://rho.http4s.org)

```scala
resolvers += Resolver.sonatypeRepo("snapshots")

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
