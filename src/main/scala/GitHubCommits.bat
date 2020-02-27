::#!
@echo off
call scala %0 %*
goto :eof
::!#
def githubRss(user: String, repo: String, branch: String): String = {
  val url = s"https://github.com/$user/$repo/commits/$branch.atom"
  val lines = io.Source.fromURL(url).getLines.toList
  val xml = lines.map(_.trim).mkString("")
  xml
}

def child(xml: String, name: String): Option[String] = {
  val p = s".*<$name>(.*)</$name>.*".r
  xml match {
    case p(result) => Option(result)
    case _ => None
  }
}

def xmlToEntryList(xml: String) = xml.split("</?entry>").filterNot(_.isEmpty).tail

def report(entryXml: String): Option[String] = {
  for {
    title <- child(entryXml, "title")
    date <- child(entryXml, "updated").map(_.replaceAll("T.*",""))
    author <- child(entryXml, "name")
  }
  yield s"title:  $title\ndate:   $date\nauthor: $author"
}

def getGithubCommitReports(urb: (String,String,String)): List[String] = {
  val xml = githubRss(urb._1, urb._2, urb._3)
  val entries = xmlToEntryList(xml).toList
  val branchInfo = s"branch: ${urb._2}:${urb._3}\n"
  entries flatMap (e => report(e).toList) map (branchInfo + _)
}

def getGithubReports(urbs: List[(String,String,String)]): String = {
  val commits = List.newBuilder[String]

  import concurrent.ExecutionContext.Implicits.global
  val futures = urbs map { urb =>
    concurrent.Future { commits ++= getGithubCommitReports(urb) }
  }
  val future = concurrent.Future.sequence(futures)

  import concurrent.duration._
  concurrent.Await.result(future, Duration(5, SECONDS))

  val separator = "\n" + "="*60 + "\n"
  val title = s"Github activity for ${urbs map (_._1) mkString (", ")} repos"

  val sortedCommits = commits.result.sortBy { c =>
    c.replaceAll("(?s).*date:   ","").replaceAll("(?s)\\s.*","")
  }.reverse

  (title :: sortedCommits) mkString separator
}


// Entry point for our script
val threefers: Array[Array[String]] = args map (_.split("/")) filter (_.size == 3)
val urbs: List[(String,String,String)] = threefers.toList map (x => (x(0), x(1), x(2)))

urbs match {
  case Nil =>
    println("Usage: GithubCommits <user>/<repo>/<branch>")
  case l =>
    println(s"Reporting for ${l.size} repositories: ${l.mkString(" & ")}")
    println(getGithubReports(l))
}