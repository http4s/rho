package com.http4s.rho.hal.plus.swagger.demo

import java.util.SortedSet

import net.sf.uadetector.datastore.DataStore
import net.sf.uadetector.internal.data.Data
import net.sf.uadetector.internal.data.domain.{
  BrowserOperatingSystemMapping,
  Browser => UBrowser,
  BrowserPattern => UBrowserPattern,
  BrowserType => UBrowserType,
  OperatingSystem => UOperatingSystem
}

import scala.jdk.CollectionConverters._
import scala.collection.immutable.Seq

// --
// Do not take this implementation too serious. It is only there to have some
// resources to play around through REST with JSON HAL
// --

case class Browser(id: Int, name: String, typeId: Int)
case class BrowserPattern(id: Int, position: Int, regex: String)
case class BrowserType(id: Int, name: String)
case class OperatingSystem(id: Int, name: String)

/** Example of an interface to get access to your business domain */
trait BusinessLayer {
  def countBrowsers: Int
  def countBrowsersByBrowserTypeId(id: Int): Int
  def countOperatingSystems: Int
  def hasBrowsersByOperatingSystemId(id: Int): Boolean
  def hasOperatingSystemsByBrowserId(id: Int): Boolean
  def findBrowser(id: Int): Option[Browser]
  def findBrowserIdByPatternId(id: Int): Option[Int]
  def findBrowserPattern(id: Int): Option[BrowserPattern]
  def findBrowserPatternsByBrowserId(id: Int): Option[Seq[BrowserPattern]]
  def findBrowserPatterns(firstResult: Int, maxResults: Int): Seq[BrowserPattern]
  def findBrowserType(id: Int): Option[BrowserType]
  def findBrowserTypes: Seq[BrowserType]
  def findBrowsers(firstResult: Int, maxResults: Int): Seq[Browser]
  def findBrowsersByBrowserTypeId(id: Int, firstResult: Int, maxResults: Int): Seq[Browser]
  def findBrowsersByOperatingSystemId(id: Int): Seq[Browser]
  def findOperatingSystem(id: Int): Option[OperatingSystem]
  def findOperatingSystems(firstResult: Int, maxResults: Int): Seq[OperatingSystem]
  def findOperatingSystemsByBrowserId(id: Int): Seq[OperatingSystem]
}

/** Implementation to get access to your business domain */
class UADetectorDatabase(val dataStore: DataStore) extends BusinessLayer {

  private def toBrowser(b: UBrowser): Browser =
    Browser(b.getId, b.getFamilyName, b.getType.getId)

  private def toBrowserPattern(b: UBrowserPattern): BrowserPattern =
    BrowserPattern(b.getId, b.getPosition, b.getPattern.pattern)

  private def toBrowserType(t: UBrowserType): BrowserType =
    BrowserType(t.getId, t.getName)

  private def toOperatingSystem(b: UOperatingSystem): OperatingSystem =
    OperatingSystem(b.getId, b.getName)

  private def data: Data =
    dataStore.getData

  private def browsers: List[Browser] =
    data.getBrowsers.asScala.foldLeft(List[Browser]()) { (acc, b) =>
      toBrowser(b) :: acc
    }

  private def browserPatterns: List[BrowserPattern] =
    data.getBrowserPatterns.asScala.foldLeft(List[BrowserPattern]()) { (acc, e) =>
      val ps = e._2.asScala.map { p =>
        toBrowserPattern(p)
      }.toList
      ps ::: acc
    }

  private def browserTypes =
    data.getBrowserTypes.asScala.map { t =>
      toBrowserType(t._2)
    }.toList

  private def operatingSystems: List[OperatingSystem] =
    data.getOperatingSystems.asScala.foldLeft(List[OperatingSystem]()) { (acc, o) =>
      toOperatingSystem(o) :: acc
    }

  def countBrowsers = data.getBrowsers.size

  def countBrowsersByBrowserTypeId(id: Int) =
    browsers.foldLeft(0) { (acc, b) =>
      if (b.typeId == id) acc + 1
      else acc
    }

  def countOperatingSystems = data.getOperatingSystems.size

  def hasBrowsersByOperatingSystemId(id: Int) = {
    val found = data.getBrowserToOperatingSystemMappings.asScala.collectFirst {
      case m: BrowserOperatingSystemMapping if m.getOperatingSystemId == id => m
    }
    found.isDefined
  }

  def hasOperatingSystemsByBrowserId(id: Int) = {
    val found = data.getBrowserToOperatingSystemMappings.asScala.collectFirst {
      case m: BrowserOperatingSystemMapping if m.getBrowserId == id => m
    }
    found.isDefined
  }

  def findBrowser(id: Int) =
    browsers.collectFirst {
      case b: Browser if b.id == id => b
    }

  def findBrowserIdByPatternId(id: Int) =
    data.getPatternToBrowserMap.entrySet.asScala.collectFirst {
      case e: java.util.Map.Entry[UBrowserPattern, UBrowser] if e.getKey.getId == id =>
        e.getValue.getId
    }

  def findBrowserPattern(id: Int) =
    browserPatterns.collectFirst {
      case p: BrowserPattern if p.id == id => p
    }

  def findBrowserPatternsByBrowserId(id: Int) =
    data.getBrowserPatterns.get(id) match {
      case ps: SortedSet[UBrowserPattern] => Some(ps.asScala.map(p => toBrowserPattern(p)).toList)
      case _ => None
    }

  def findBrowserPatterns(firstResult: Int, maxResults: Int) =
    browserPatterns.drop(firstResult).take(maxResults)

  def findBrowserType(id: Int) =
    browserTypes.collectFirst {
      case b: BrowserType if b.id == id => b
    }

  def findBrowserTypes =
    browserTypes

  def findBrowsers(firstResult: Int, maxResults: Int) =
    browsers.drop(firstResult).take(maxResults)

  def findBrowsersByBrowserTypeId(id: Int, firstResult: Int, maxResults: Int) = {
    val matchingBrowsers = for {
      browser <- browsers
      if browser.typeId == id
    } yield browser
    matchingBrowsers.drop(firstResult).take(maxResults)
  }

  def findBrowsersByOperatingSystemId(id: Int) = {
    val matchingBrowsers = for {
      mapping <- data.getBrowserToOperatingSystemMappings.asScala
      if mapping.getOperatingSystemId == id
      browser <- browsers
      if browser.id == mapping.getBrowserId
    } yield browser
    matchingBrowsers.toList
  }

  def findOperatingSystem(id: Int) =
    operatingSystems.collectFirst {
      case o: OperatingSystem if o.id == id => o
    }

  def findOperatingSystems(firstResult: Int, maxResults: Int) =
    operatingSystems.drop(firstResult).take(maxResults)

  def findOperatingSystemsByBrowserId(id: Int) = {
    val matchingOperatingSystems = for {
      mapping <- data.getBrowserToOperatingSystemMappings.asScala
      if mapping.getBrowserId == id
      os <- operatingSystems
      if os.id == mapping.getOperatingSystemId
    } yield os
    matchingOperatingSystems.toList
  }

}
