### How to use the provided examples

Everything runs from *Simple Build Tool* (sbt) by starting it in parent project's directory.

If *sbt* is started type
```
rho-examples/reStart
```
and hit enter. You get asked which example you want to execute. Choose one of them by typing the a number. If the server is started you will see the message "Starting Http4s-blaze example on '8080'" in your console. Then you can access via [http://localhost:8080]().

You can stop the running server by executing
```
rho-examples/reStop
```

The JSON HAL example contains a JavaScript-based browser to easily navigate from one resource to another. Just enter [http://localhost:8080/hal-ui]() to get access.

Both examples contains a Swagger UI to make the automatically generated API more accessible for humans. Visit [http://localhost:8080/swagger-ui]() to navigate through the REST API documentation of the running example.