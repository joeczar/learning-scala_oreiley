

object RssRight extends App {

  def gitHubRss(user: String, repo: String, branch: String): String = {
    val url = s"https://github.com/$user/$repo/commits/$branch.atom"
    val lines = io.Source.fromURL(url).getLines.toList
    val xml = lines.map(_.trim).mkString("")
    xml
  }

  def xmlToEntryList(xml: String) =
    xml.split("</?entry>").filterNot(_.isEmpty).tail

//  val entries = xmlToEntryList(xml)
 // println(s"Got ${entries.size} entries")
  //entries.foreach(x => println(x))

  def child(xml: String, name: String): Option[String] = {
    val p = s".*<$name>(.*)</$name>.*".r
    xml match {
      case p(result) => Option(result)
      case _ => None
    }
  }
  //println( child(entries(2), "date"))

  def report(entryXML: String): Option[String] = for {
    title <- child(entryXML, "title")
    date <- child(entryXML, "updated").map(_.replaceAll("T.*", ""))
    author <- child(entryXML, "name")
  }
    yield s"Title: $title\nDate: $date\nAuthor: $author"

  //println(report(entries(1)))
  def gitHubReport(urb: (String,  String,  String)): String = {
    val xml = gitHubRss(urb._1, urb._2, urb._3)
    val entries = xmlToEntryList(xml).toList
    val formattedEntries = entries flatMap report
    val title = s"GitHub commit activity for ${urb._2}:${urb._3}"
    title :: formattedEntries mkString ("\n" + "=" * 80 + "\n")
  }
  def gitHubCommitReports(urb: (String,  String,  String)): List[String] = {
    val xml = gitHubRss(urb._1, urb._2, urb._3)
    val entries = xmlToEntryList(xml).toList
    val branchInfo = s"branch: ${urb._2}:${urb._3}\n"
    entries flatMap report map (branchInfo + _)

  }
/*
  def gitHubReports(urbs: List[(String, String, String)]) = {
    val commits = urbs flatMap gitHubCommitReports
    val separator = "\n" + "="*60 + "\n"
    val title = s"GitHub activity for ${urbs map (_._1) mkString (", ")} repos"
    title :: commits mkString separator
  }

 */

  def gitHubReports(urbs: List[(String, String, String)]): String = {
    val commits = List.newBuilder[String]

    import concurrent.ExecutionContext.Implicits.global

    val futures = urbs map { urb =>
      concurrent.Future { commits ++= gitHubCommitReports(urb) }
    }
    val future = concurrent.Future.sequence(futures)
    import concurrent.duration._
    concurrent.Await.result(future, Duration(5, SECONDS))

    val separator = "\n" + "="*60 + "\n"
    val title = s"GitHub activity for ${urbs map (_._1) mkString (", ")} repos"

    val sortedCommits = commits.result.sortBy{ c =>
      c.replaceAll("(?s).*date: ", "").replaceAll("(?s)\\s.*", "")
    }.reverse

    (title :: sortedCommits) mkString separator

  }

  val scalaUdemyReport = ("joeczar", "scalaUdemy", "master")
  val slickUrb = ("slick","slick","master")
  val akkaUrb = ("akka","akka","master")
  val scalaUrb = ("scala","scala","2.11.x")
  val scalazUrb = ("scalaz","scalaz","series/7.2.x")
  val repoList = List(scalaUdemyReport, slickUrb, akkaUrb, scalaUrb, scalazUrb)

  println(gitHubReports(repoList))
}


