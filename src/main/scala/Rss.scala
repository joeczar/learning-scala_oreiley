
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
//import scala.concurrent.{Await, Future}

object Rss extends App {

  def getRSSFeed(url: String): Future[String] = Future {
    val raw = io.Source.fromURL(url)
    raw.getLines.map(_.trim).mkString("")
  }
  val RSS_StringAwait: String = Await.result( // this works
    getRSSFeed("https://github.com/scala/scala/commits/2.11.x.atom"), 5.seconds
  )

  def xmlParser(input: String): Vector[Map[String, String]]= {
    println(s"parsing....\n ")
    val keyPattern = """(<[^>]+>)([0-9a-zA-Z-#()/.=$";/:\-`´ ]+)""".r

    val matches = for {
      patternMatch <- keyPattern.findAllMatchIn(input)

    }yield {
      Map(patternMatch.group(1).replaceAll("[<>]", "") -> patternMatch.group(2) )
    }
    matches.toVector
  }

  val getRSSFutureString = getRSSFeed("https://github.com/scala/scala/commits/2.11.x.atom")
    .map{
       x => val result = xmlParser(x); result
    }

  //getRSSFutureString.foreach(vector => vector.map(maps => println(maps)))

  println {
    getRSSFutureString
  }
  val rssVectorMaps = xmlParser(RSS_StringAwait)
  println(rssVectorMaps(0)("id") )
  // This is the point where I give up and copy the answer.

}
