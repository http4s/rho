# ρ: A DSL for building HTTP services with http4s

```scala
val httpService = new RhoService {
   GET / "hello" / 'world +? param[Int]("fav") |>> { (world: String, fav: Int) => 
     Ok(s"Received $fav, $world") 
   }
}
```
See the [tests](https://github.com/http4s/rho/blob/master/core/src/test/scala/org/http4s/rhotest/ApiExamples.scala) for more examples.

### Get more from your route definitions
The primary goal of ρ is to provide an easy to use AST with which to build HTTP services which can be inspected to extract a variety of information including:
* [Swagger documentation](http://swagger.wordnik.com/)
* HAL documentation

### Get ρ
Rho artifacts are available at Maven Central and snapshots are available from the Sonatype repositories
Read the [Rho Scaladocs](http://rho.http4s.org)

```scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.http4s" %% "rho-swagger" % version

```

### Stability
ρ remains a work in progress.

### Contributing
Contributions of all kinds are welcome! Pull requests are greatly appreciated from their original authors, licensed to the http4s project under the project's [open source license](https://github.com/http4s/http4s/blob/master/LICENSE).

### License
ρ is licensed under the terms of the Apache 2.0 license. See the license file in the base directory for more information.
