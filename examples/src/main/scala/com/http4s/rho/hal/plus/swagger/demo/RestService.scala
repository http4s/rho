package com.http4s.rho.hal.plus.swagger.demo

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe

import org.http4s.Request
import org.http4s.Uri
import org.http4s.rho.RhoService
import org.http4s.rho.hal._
import org.http4s.rho.hal.{ ResourceObjectBuilder => ResObjBuilder }
import org.http4s.rho._
import org.http4s.rho.swagger._

import scalaz.{ -\/, \/- }

class RestService(val businessLayer: BusinessLayer) extends RhoService with SwaggerSupport {

  // # Query Parameters

  val firstResult = param[Int]("firstResult", 0, (i: Int) => i >= 0)
  val maxResults = param[Int]("maxResults", 10, (i: Int) => i >= 1 && i <= 100)
  val owner = param[String]("owner", "")
  val groups = param[Seq[String]]("groups", Nil)
  val searchTerm = param[String]("searchTerm", "")
  val sortBy = param[String]("sortBy", "")
  val showHidden = param[Int]("showHidden", 0)

  // # Path Variables

  val id = pathVar[Int]("id")

  // # HTTP Routes

  val browsers = "browsers"
  GET / browsers +? firstResult & maxResults |>> { (request: Request, first: Int, max: Int) =>
    val self = request.uri
    val configurations = businessLayer.findBrowsers(first, max)
    val total = businessLayer.countBrowsers
    val hal = browsersAsResource(self, first, max, configurations, total)
    Ok(hal.build)
  }

  val browserById = browsers / id
  GET / browserById |>> { id: Int =>
    val found = for { browser <- businessLayer.findBrowser(id) } yield {
      val b = browserAsResourceObject(browser)
      if (businessLayer.hasOperatingSystemsByBrowserId(browser.id))
        for (tpl <- operatingSystemsByBrowser.asUriTemplate)
          b.link("operating-systems", tpl.expandPath("id", browser.id).toUriIfPossible.get)

      \/-(Ok(b.build))
    }
    found getOrElse -\/(NotFound(warning(s"Browser $id not found")))
  }

  val browserPatternsById = browsers / id / "patterns"
  GET / browserPatternsById |>> { (request: Request, id: Int) =>
    val self = request.uri
    val found = for { patterns <- businessLayer.findBrowserPatternsByBrowserId(id) }
      yield \/-(Ok(browserPatternsAsResource(self, 0, Int.MaxValue, patterns, patterns.size).build))
    found getOrElse -\/(NotFound(warning(s"Browser $id not found")))
  }

  val browserPatterns = "browser-patterns"
  GET / browserPatterns +? firstResult & maxResults |>> { (request: Request, first: Int, max: Int) =>
    val self = request.uri
    val patterns = businessLayer.findBrowserPatterns(first, max)
    val total = businessLayer.countBrowsers
    val hal = browserPatternsAsResource(self, first, max, patterns, total)
    Ok(hal.build)
  }

  val browserPatternById = browserPatterns / id
  GET / browserPatternById |>> { id: Int =>
    val found = for { pattern <- businessLayer.findBrowserPattern(id) } yield {
      val b = browserPatternAsResourceObject(pattern)
      for {
        tpl <- browserById.asUriTemplate
        browserId <- businessLayer.findBrowserIdByPatternId(pattern.id)
      } b.link("browser", tpl.expandPath("id", browserId).toUriIfPossible.get)
      \/-(Ok(b.build))
    }
    found getOrElse -\/(NotFound(warning(s"Browser $id not found")))
  }

  val browserTypes = "browser-types"
  GET / browserTypes |>> { (request: Request) =>
    val self = request.uri
    val types = businessLayer.findBrowserTypes
    val hal = browserTypesAsResource(self, types)
    Ok(hal.build)
  }

  val browserTypeById = browserTypes / id
  GET / browserTypeById |>> { id: Int =>
    val found = for { browserType <- businessLayer.findBrowserType(id) } yield {
      val b = browserTypeAsResourceObject(browserType)
      for {
        tpl <- browsersByBrowserTypeId.asUriTemplate
      } b.link("browsers", tpl.expandPath("id", browserType.id).toUriIfPossible.get)
      \/-(Ok(b.build))
    }
    found getOrElse -\/(NotFound(warning(s"Browser type $id not found")))
  }

  val browsersByBrowserTypeId = browserTypes / id / "browsers"
  GET / browsersByBrowserTypeId +? firstResult & maxResults |>> { (request: Request, id: Int, first: Int, max: Int) =>
    val self = request.uri
    val browsers = businessLayer.findBrowsersByBrowserTypeId(id, first, max)
    val total = businessLayer.countBrowsersByBrowserTypeId(id)
    if (browsers.nonEmpty)
      \/-(Ok(browsersAsResource(self, first, max, browsers, total).build))
    else
      -\/(NotFound(warning(s"No browsers for type $id found")))
  }

  val operatingSystems = "operating-systems"
  GET / operatingSystems +? firstResult & maxResults |>> { (request: Request, first: Int, max: Int) =>
    val self = request.uri
    val configurations = businessLayer.findOperatingSystems(first, max)
    val total = businessLayer.countOperatingSystems
    val hal = operatingSystemsAsResource(self, first, max, configurations, total)
    Ok(hal.build)
  }

  val operatingSystemById = operatingSystems / id
  GET / operatingSystemById |>> { id: Int =>
    val found = for { operatingSystem <- businessLayer.findOperatingSystem(id) } yield {
      val b = operatingSystemAsResourceObject(operatingSystem)
      if (businessLayer.hasBrowsersByOperatingSystemId(operatingSystem.id))
        for (tpl <- browsersByOperatingSystem.asUriTemplate)
          b.link("browsers", tpl.expandPath("id", operatingSystem.id).toUriIfPossible.get)
      \/-(Ok(b.build))
    }
    found getOrElse -\/(NotFound(warning(s"OperatingSystem $id not found")))
  }

  val browsersByOperatingSystem = operatingSystemById / "browsers"
  GET / browsersByOperatingSystem |>> { (request: Request, id: Int) =>
    val self = request.uri
    val browsers = businessLayer.findBrowsersByOperatingSystemId(id)
    if (browsers.nonEmpty)
      \/-(Ok(browsersAsResource(self, 0, Int.MaxValue, browsers, browsers.size).build))
    else
      -\/(NotFound(warning(s"No Browsers for operating system $id found")))
  }

  val operatingSystemsByBrowser = browserById / "operating-systems"
  GET / operatingSystemsByBrowser |>> { (request: Request, id: Int) =>
    val self = request.uri
    val operatingSystems = businessLayer.findOperatingSystemsByBrowserId(id)
    if (operatingSystems.nonEmpty)
      \/-(Ok(operatingSystemsAsResource(self, 0, Int.MaxValue, operatingSystems, operatingSystems.size).build))
    else
      -\/(NotFound(warning(s"No operating systems for browser $id found")))
  }

  GET / "" |>> { request: Request =>
    val b = new ResObjBuilder[Nothing, Nothing]()
    b.link("self", request.uri)
    for (uri <- browsers.asUri) b.link(browsers, uri.toString, "Lists browsers")
    for (uri <- browserPatterns.asUri) b.link(browserPatterns, uri.toString, "Lists browser patterns")
    for (uri <- browserTypes.asUri) b.link(browserTypes, uri.toString, "Lists browser types")
    for (uri <- operatingSystems.asUri) b.link(operatingSystems, uri.toString, "Lists operating systems")
    Ok(b.build)
  }

  // # JSON HAL helpers

  def browsersAsResource(self: Uri, first: Int, max: Int, browsers: Seq[Browser], total: Int): ResObjBuilder[(String, Long), Browser] = {
    val hal = new ResObjBuilder[(String, Long), Browser]()
    hal.link("self", selfWithFirstAndMax(self, first, max))
    hal.content("total", total)
    if (first + max < total) {
      hal.link("next", self +? (firstResult, first + max) +? (maxResults, max))
    }
    if (first > 0) {
      hal.link("prev", self +? (firstResult, Math.max(first - max, 0)) +? (maxResults, max))
    }
    val res = ListBuffer[ResourceObject[Browser, Nothing]]()
    browsers.foreach { browser =>
      res.append(browserAsResourceObject(browser).build)
    }
    hal.resources("browsers", res.toList)
  }

  def browserAsResourceObject(browser: Browser): ResObjBuilder[Browser, Nothing] = {
    val b = new ResObjBuilder[Browser, Nothing]()
    for (tpl <- browserById.asUriTemplate)
      b.link("self", tpl.expandPath("id", browser.id).toUriIfPossible.get)
    for (tpl <- browserPatternsById.asUriTemplate)
      b.link("patterns", tpl.expandPath("id", browser.id).toUriIfPossible.get)
    for (tpl <- browserTypeById.asUriTemplate)
      b.link("type", tpl.expandPath("id", browser.typeId).toUriIfPossible.get)
    b.content(browser)
  }

  def browserPatternsAsResource(self: Uri, first: Int, max: Int, browserPatterns: Seq[BrowserPattern], total: Int): ResObjBuilder[(String, Long), BrowserPattern] = {
    val hal = new ResObjBuilder[(String, Long), BrowserPattern]()
    hal.link("self", selfWithFirstAndMax(self, first, max))
    hal.content("total", total)
    if (first + max < total) {
      hal.link("next", self +? (firstResult, first + max) +? (maxResults, max))
    }
    if (first > 0) {
      hal.link("prev", self +? (firstResult, Math.max(first - max, 0)) +? (maxResults, max))
    }
    val res = ListBuffer[ResourceObject[BrowserPattern, Nothing]]()
    browserPatterns.foreach { browserPattern =>
      res.append(browserPatternAsResourceObject(browserPattern).build)
    }
    hal.resources("browserPatterns", res.toList)
  }

  def browserPatternAsResourceObject(browserPattern: BrowserPattern): ResObjBuilder[BrowserPattern, Nothing] = {
    val b = new ResObjBuilder[BrowserPattern, Nothing]()
    for (tpl <- browserPatternById.asUriTemplate)
      b.link("self", tpl.expandPath("id", browserPattern.id).toUriIfPossible.get)
    b.content(browserPattern)
  }

  def browserTypeAsResourceObject(browserType: BrowserType): ResObjBuilder[BrowserType, Nothing] = {
    val b = new ResObjBuilder[BrowserType, Nothing]()
    for (tpl <- browserTypeById.asUriTemplate)
      b.link("self", tpl.expandPath("id", browserType.id).toUriIfPossible.get)
    b.content(browserType)
  }

  def browserTypesAsResource(self: Uri, browserTypes: Seq[BrowserType]): ResObjBuilder[Nothing, BrowserType] = {
    val hal = new ResObjBuilder[Nothing, BrowserType]()
    hal.link("self", self)
    val res = ListBuffer[ResourceObject[BrowserType, Nothing]]()
    browserTypes.foreach { browserType =>
      res.append(browserTypeAsResourceObject(browserType).build)
    }
    hal.resources("browserTypes", res.toList)
  }

  def operatingSystemsAsResource(self: Uri, first: Int, max: Int, operatingSystems: Seq[OperatingSystem], total: Int): ResObjBuilder[(String, Long), OperatingSystem] = {
    val hal = new ResObjBuilder[(String, Long), OperatingSystem]()
    hal.link("self", selfWithFirstAndMax(self, first, max))
    hal.content("total", total)
    if (first + max < total) {
      hal.link("next", self +? (firstResult, first + max) +? (maxResults, max))
    }
    if (first > 0) {
      hal.link("prev", self +? (firstResult, Math.max(first - max, 0)) +? (maxResults, max))
    }
    val res = ListBuffer[ResourceObject[OperatingSystem, Nothing]]()
    operatingSystems.foreach { operatingSystem =>
      res.append(operatingSystemAsResourceObject(operatingSystem).build)
    }
    hal.resources("operatingSystems", res.toList)
  }

  def operatingSystemAsResourceObject(operatingSystem: OperatingSystem): ResObjBuilder[OperatingSystem, Nothing] = {
    val b = new ResObjBuilder[OperatingSystem, Nothing]()
    for (tpl <- operatingSystemById.asUriTemplate)
      b.link("self", tpl.expandPath("id", operatingSystem.id).toUriIfPossible.get)
    b.content(operatingSystem)
  }

  def selfWithFirstAndMax(self: Uri, first: Int, max: Int): Uri = {
    if (!self.containsQueryParam(firstResult) && !self.containsQueryParam(maxResults)) self
    else self +? (firstResult, first) +? (maxResults, max)
  }

  // use JSON messages if a non-successful HTTP status must be send 

  def message(text: String, `type`: MessageType): Message = {
    Message(text, `type`)
  }
  def error(text: String): Message = message(text, Error)
  def info(text: String):  Message = message(text, Info)
  def warning(text: String):  Message = message(text, Warning)

}
