package org.http4s.rho.hal

import org.specs2.mutable.Specification

class LinkObjectSpec extends Specification {

  "LinkObject" should {
    "have a non-null href" in {
      new LinkObject(null) must throwA[IllegalArgumentException]
    }
    "have a non-empty href" in {
      new LinkObject("") must throwA[IllegalArgumentException]
    }
    "require only a href property" in {
      new LinkObject("/link") must be equalTo new LinkObject("/link")
    }
    "have a templated property optionally" in {
      new LinkObject("/link", templated = Some(true)).templated.get must beTrue
    }
    "have a type property optionally" in {
      new LinkObject("/link", `type` = Some("application/json")).`type`.get must be equalTo "application/json"
    }
    "have a deprecation property optionally" in {
      new LinkObject("/link", deprecation = Some("http://more/info/about/deprecated")).deprecation.get must be equalTo "http://more/info/about/deprecated"
    }
    "have a name property optionally" in {
      new LinkObject("/link", name = Some("Max")).name.get must be equalTo "Max"
    }
    "have a profile property optionally" in {
      new LinkObject("/link", profile = Some("profile1")).profile.get must be equalTo "profile1"
    }
    "have a title property optionally" in {
      new LinkObject("/link", title = Some("The case for hyperlinks in APIs")).title.get must be equalTo "The case for hyperlinks in APIs"
    }
    "have a hreflang property optionally" in {
      new LinkObject("/link", hreflang = Some("/href/lang")).hreflang.get must be equalTo "/href/lang"
    }
    "have empty optional properties per default" in {
      new LinkObject("/link") must be equalTo new LinkObject("/link", None, None, None, None, None, None, None)
    }
  }

}