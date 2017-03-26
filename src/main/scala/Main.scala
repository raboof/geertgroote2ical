import java.io.{ InputStream, OutputStream }
import java.nio.charset.Charset
import java.time._

import com.amazonaws.services.lambda.runtime.{ Context, RequestStreamHandler }

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }

import dispatch.Http

import scala.concurrent._
import scala.concurrent.duration._

import icalendar._
import icalendar.Properties._
import icalendar.CalendarProperties._
import icalendar.ValueTypes._
import icalendar.ical.Writer._

import net.ruippeixotog.scalascraper.model._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.scraper.HtmlExtractor

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

object Helpers {
  implicit class RegexHelper(val sc: StringContext) extends AnyVal {
    def re: scala.util.matching.Regex = sc.parts.mkString.r
  }
  object ToInt {
    def unapply(in: String): Option[Int] = Try(in.toInt).toOption
  }
}

trait Main {
  import Helpers._

  implicit val ec: ectrace.WrappedExecutionContext = ectrace.WrappedExecutionContext(ExecutionContext.global)
  implicit def liftOption[T](value: T): Option[T] = Some(value)

  def links(doc: Document): List[String] =
    doc >> elementList("article header h2 a") >> attr("href")("a")

  object ToMonth {
    val months = Map(
    "Jan" -> Month.JANUARY,
    "Feb" -> Month.FEBRUARY,
    "Mrt" -> Month.MARCH,
    "Apr" -> Month.APRIL,
    "Mei" -> Month.MAY,
    "Jun" -> Month.JUNE,
    "Jul" -> Month.JULY,
    "Aug" -> Month.AUGUST,
    "Sep" -> Month.SEPTEMBER,
    "Okt" -> Month.OCTOBER,
    "Nov" -> Month.NOVEMBER,
    "Dec" -> Month.DECEMBER
    )
    def unapply(in: String): Option[Month] = months.get(in)
  }

  def parseEvent(url: String, doc: Document): Event = {
    val article = doc >> element("article")
    val id = (article >> attr("id")("article")).split("-")(1)
    val start = (article >> text("h3")) match {
      case re"\s*(\d+)${ToInt(day)} (\w+)${ToMonth(month)} (\d+)${ToInt(year)} \((\d+)${ToInt(hour)}:(\d+)${ToInt(minute)}.*" =>
        LocalDateTime.of(year, month, day, hour, minute).atZone(ZoneId.of("Europe/Amsterdam"))
    }
    val title = doc >> text("section[class=row beam] h1")
    val paragraph = article >?> text("p")

    Event(
      uid = Uid(s"geertgroote2ical-$id"),
      dtstart = start,
      summary = Summary(title),
      description = paragraph.map(Description(_)),
      url = Url(url)
    )
  }

  def fetchDocument(uri: String): Future[Document] = {
    Http(dispatch.url(uri) OK dispatch.as.String).map(JsoupBrowser().parseString(_))
  }

  def event(url: String): Future[Event] = {
    fetchDocument(url).flatMap(doc => Try(parseEvent(url, doc)) match {
      case Success(evt) => Future.successful(evt)
      case Failure(t) => Future.failed(new IllegalStateException(s"Failed to parse $url", t))
    })
  }

  def fetchCalendar(): String = {
    val events = fetchDocument("http://www.geertgrootehuis.nl/agenda/")
      .map(agenda => links(agenda))
      .flatMap(links => Future.sequence(links.map(event(_))))

    asIcal(Calendar(
      prodid = Prodid("-//raboof/geertgrote2ical//NONSGML v1.0//NL"),
      events = Await.result(events, 120 seconds)
    ))
  }
}

class MainLambda extends Main {
  def handleRequest(inputStream: InputStream, outputStream: OutputStream, context: Context): Unit = {
    context.getLogger().log("starting\n")
    val result = fetchCalendar()
    outputStream.write(result.getBytes("UTF-8"));
    outputStream.flush();
    context.getLogger().log("returning\n")
  }
}

object MainApp extends App with Main {
  try {
    print(fetchCalendar())
  } finally {
    dispatch.Http.shutdown()
  }
}
