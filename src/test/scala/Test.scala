import java.time.{ZonedDateTime, ZoneId}

import icalendar.ical.Writer._
import icalendar.Properties._

import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import org.scalatest._

class Test extends WordSpec with Matchers with Main {
  "The HTML scraping algorithm" should {
    val browser = JsoupBrowser()

    "correctly find links in a yearly overview page" in {
      val doc = browser.parseResource("/agenda.html")
      links(doc).size should be(10)
      links(doc)(0) should be("http://www.geertgrootehuis.nl/agenda/stadsverlichting-deventer/")
    }

    "correctly convert a details page to an event" in {
      val doc = browser.parseResource("/stadsverlichting.html")
      val event = parseEvent("http://www.geertgrootehuis.nl/agenda/stadsverlichting-deventer/", doc)
      event.uid.value.text should be("geertgroote2ical-884")
      event.dtstart.get.value.dt should be(ZonedDateTime.of(2017, 2, 12, 17, 0, 0, 0, ZoneId.of("Europe/Amsterdam")))
      event.summary.get.value.text should be("Stadsverlichting Deventer")
    }

    "correctly convert a details page without description to an event" in {
      val doc = browser.parseResource("/principes.html")
      val event = parseEvent("http://www.geertgrootehuis.nl/agenda/socratisch-gesprek-principes/", doc)
      event.uid.value.text should be("geertgroote2ical-883")
      event.dtstart.get.value.dt should be(ZonedDateTime.of(2017, 2, 14, 13, 30, 0, 0, ZoneId.of("Europe/Amsterdam")))
      event.summary.get.value.text should be("Socratisch gesprek over Principes")
      event.description should be(None)
    }

    "parse gosia-kalisciak-exposeert.html" in {
      val doc = browser.parseResource("/gosia-kalisciak-exposeert.html")
      val event = parseEvent("http://www.geertgrootehuis.nl/agenda/gosia-kalisciak-exposeert/", doc)
    }
  }
}
