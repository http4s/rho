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
  GET / browsers +? firstResult & maxResults |>> Action { (request: Request, first: Int, max: Int) =>
    val configurations = businessLayer.findBrowsers(first, max)
    val total = businessLayer.countBrowsers
    val hal = browsersAsResource(request, first, max, configurations, total)
    Ok(hal.build)
  }

  val browserById = browsers / id
  GET / browserById |>> Action { (request: Request, id: Int) =>
    val found = for { browser <- businessLayer.findBrowser(id) } yield {
      val b = browserAsResourceObject(browser, request)
      if (businessLayer.hasOperatingSystemsByBrowserId(browser.id))
        for (tpl <- operatingSystemsByBrowser.asUriTemplate(request))
          b.link("operating-systems", tpl.expandPath("id", browser.id).toUriIfPossible.get)

      Ok(b.build)
    }
    found getOrElse NotFound(warning(s"Browser $id not found"))
  }

  val browserPatternsById = browsers / id / "patterns"
  GET / browserPatternsById |>> Action { (request: Request, id: Int) =>
    val found = for { patterns <- businessLayer.findBrowserPatternsByBrowserId(id) }
      yield Ok(browserPatternsAsResource(request, 0, Int.MaxValue, patterns, patterns.size).build)
    found getOrElse NotFound(warning(s"Browser $id not found"))
  }

  val browserPatterns = "browser-patterns"
  GET / browserPatterns +? firstResult & maxResults |>> Action { (request: Request, first: Int, max: Int) =>
    val patterns = businessLayer.findBrowserPatterns(first, max)
    val total = businessLayer.countBrowsers
    val hal = browserPatternsAsResource(request, first, max, patterns, total)
    Ok(hal.build)
  }

  val browserPatternById = browserPatterns / id
  GET / browserPatternById |>> Action { (request: Request, id: Int) =>
    val found = for { pattern <- businessLayer.findBrowserPattern(id) } yield {
      val b = browserPatternAsResourceObject(pattern, request)
      for {
        tpl <- browserById.asUriTemplate(request)
        browserId <- businessLayer.findBrowserIdByPatternId(pattern.id)
      } b.link("browser", tpl.expandPath("id", browserId).toUriIfPossible.get)
      Ok(b.build)
    }
    found getOrElse NotFound(warning(s"Browser $id not found"))
  }

  val browserTypes = "browser-types"
  GET / browserTypes |>> Action { (request: Request) =>
    val types = businessLayer.findBrowserTypes
    val hal = browserTypesAsResource(request, types)
    Ok(hal.build)
  }

  val browserTypeById = browserTypes / id
  GET / browserTypeById |>> Action { (request: Request, id: Int) =>
    val found = for { browserType <- businessLayer.findBrowserType(id) } yield {
      val b = browserTypeAsResourceObject(browserType, request)
      for {
        tpl <- browsersByBrowserTypeId.asUriTemplate(request)
      } b.link("browsers", tpl.expandPath("id", browserType.id).toUriIfPossible.get)
      Ok(b.build)
    }
    found getOrElse NotFound(warning(s"Browser type $id not found"))
  }

  val browsersByBrowserTypeId = browserTypes / id / "browsers"
  GET / browsersByBrowserTypeId +? firstResult & maxResults |>> Action { (request: Request, id: Int, first: Int, max: Int) =>
    val browsers = businessLayer.findBrowsersByBrowserTypeId(id, first, max)
    val total = businessLayer.countBrowsersByBrowserTypeId(id)
    if (browsers.nonEmpty)
      Ok(browsersAsResource(request, first, max, browsers, total).build)
    else
      NotFound(warning(s"No browsers for type $id found"))
  }

  val operatingSystems = "operating-systems"
  GET / operatingSystems +? firstResult & maxResults |>> Action { (request: Request, first: Int, max: Int) =>
    val configurations = businessLayer.findOperatingSystems(first, max)
    val total = businessLayer.countOperatingSystems
    val hal = operatingSystemsAsResource(request, first, max, configurations, total)
    Ok(hal.build)
  }

  val operatingSystemById = operatingSystems / id
  GET / operatingSystemById |>> Action { (request: Request, id: Int) =>
    val found = for { operatingSystem <- businessLayer.findOperatingSystem(id) } yield {
      val b = operatingSystemAsResourceObject(operatingSystem, request)
      if (businessLayer.hasBrowsersByOperatingSystemId(operatingSystem.id))
        for (tpl <- browsersByOperatingSystem.asUriTemplate(request))
          b.link("browsers", tpl.expandPath("id", operatingSystem.id).toUriIfPossible.get)
      Ok(b.build)
    }
    found getOrElse NotFound(warning(s"OperatingSystem $id not found"))
  }

  val browsersByOperatingSystem = operatingSystemById / "browsers"
  GET / browsersByOperatingSystem |>> Action { (request: Request, id: Int) =>
    val browsers = businessLayer.findBrowsersByOperatingSystemId(id)
    if (browsers.nonEmpty)
      Ok(browsersAsResource(request, 0, Int.MaxValue, browsers, browsers.size).build)
    else
      NotFound(warning(s"No Browsers for operating system $id found"))
  }

  val operatingSystemsByBrowser = browserById / "operating-systems"
  GET / operatingSystemsByBrowser |>> Action { (request: Request, id: Int) =>
    val operatingSystems = businessLayer.findOperatingSystemsByBrowserId(id)
    if (operatingSystems.nonEmpty)
      Ok(operatingSystemsAsResource(request, 0, Int.MaxValue, operatingSystems, operatingSystems.size).build)
    else
      NotFound(warning(s"No operating systems for browser $id found"))
  }

  GET / "" |>> Action { request: Request =>
    val b = new ResObjBuilder[Nothing, Nothing]()
    b.link("self", request.uri)
    for (uri <- browsers.asUri(request)) b.link(browsers, uri.toString, "Lists browsers")
    for (uri <- browserPatterns.asUri(request)) b.link(browserPatterns, uri.toString, "Lists browser patterns")
    for (uri <- browserTypes.asUri(request)) b.link(browserTypes, uri.toString, "Lists browser types")
    for (uri <- operatingSystems.asUri(request)) b.link(operatingSystems, uri.toString, "Lists operating systems")
    Ok(b.build)
  }

  // # JSON HAL helpers

  def browsersAsResource(request: Request, first: Int, max: Int, browsers: Seq[Browser], total: Int): ResObjBuilder[(String, Long), Browser] = {
    val self = request.uri
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
      res.append(browserAsResourceObject(browser, request).build)
    }
    hal.resources("browsers", res.toList)
  }

  def browserAsResourceObject(browser: Browser, request: Request): ResObjBuilder[Browser, Nothing] = {
    val b = new ResObjBuilder[Browser, Nothing]()
    for (tpl <- browserById.asUriTemplate(request))
      b.link("self", tpl.expandPath("id", browser.id).toUriIfPossible.get)
    for (tpl <- browserPatternsById.asUriTemplate(request))
      b.link("patterns", tpl.expandPath("id", browser.id).toUriIfPossible.get)
    for (tpl <- browserTypeById.asUriTemplate(request))
      b.link("type", tpl.expandPath("id", browser.typeId).toUriIfPossible.get)
    b.content(browser)
  }

  def browserPatternsAsResource(request: Request, first: Int, max: Int, browserPatterns: Seq[BrowserPattern], total: Int): ResObjBuilder[(String, Long), BrowserPattern] = {
    val self = request.uri
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
      res.append(browserPatternAsResourceObject(browserPattern, request).build)
    }
    hal.resources("browserPatterns", res.toList)
  }

  def browserPatternAsResourceObject(browserPattern: BrowserPattern, request: Request): ResObjBuilder[BrowserPattern, Nothing] = {
    val b = new ResObjBuilder[BrowserPattern, Nothing]()
    for (tpl <- browserPatternById.asUriTemplate(request))
      b.link("self", tpl.expandPath("id", browserPattern.id).toUriIfPossible.get)
    b.content(browserPattern)
  }

  def browserTypeAsResourceObject(browserType: BrowserType, request: Request): ResObjBuilder[BrowserType, Nothing] = {
    val b = new ResObjBuilder[BrowserType, Nothing]()
    for (tpl <- browserTypeById.asUriTemplate(request))
      b.link("self", tpl.expandPath("id", browserType.id).toUriIfPossible.get)
    b.content(browserType)
  }

  def browserTypesAsResource(request: Request, browserTypes: Seq[BrowserType]): ResObjBuilder[Nothing, BrowserType] = {
    val self = request.uri
    val hal = new ResObjBuilder[Nothing, BrowserType]()
    hal.link("self", self)
    val res = ListBuffer[ResourceObject[BrowserType, Nothing]]()
    browserTypes.foreach { browserType =>
      res.append(browserTypeAsResourceObject(browserType, request).build)
    }
    hal.resources("browserTypes", res.toList)
  }

  def operatingSystemsAsResource(request: Request, first: Int, max: Int, operatingSystems: Seq[OperatingSystem], total: Int): ResObjBuilder[(String, Long), OperatingSystem] = {
    val self = request.uri
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
      res.append(operatingSystemAsResourceObject(operatingSystem, request).build)
    }
    hal.resources("operatingSystems", res.toList)
  }

  def operatingSystemAsResourceObject(operatingSystem: OperatingSystem, request: Request): ResObjBuilder[OperatingSystem, Nothing] = {
    val b = new ResObjBuilder[OperatingSystem, Nothing]()
    for (tpl <- operatingSystemById.asUriTemplate(request))
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
