import scala.io.Source
object Collections extends App {

  def reduceOp[A, B](l: List[A], start: B)(f: (B, A) => B): B = {
     var a = start
     for (i <- l) a = f(a, i)
     a
       }


  /*
    1. Create a list of the first 20 odd Long numbers. Can you create this with a for-loop, with the filter operation,
    and with the map operation? What’s the most efficient and expressive way to write this?
    */
  val twentyOddRange:List[Long] = List.range(1L,20L, 2)
  val twentyOddFor: List[Long]= (for(i <- 1L to 20; if i % 2 != 0) yield i).toList
  val twentyOddFilter: List[Long] = List.range(0L, 20L).filter(_ % 2 != 0)
  val oddFilter = (x: Long) => x % 2 != 0
  //val twentyOddMap: List[Long] = List.range(0L, 20L).map(i =>oddFilter)

  // solutions were not lists but vectors...
  val solMap = 0L to 9L map (_ * 2 + 1)

  println(twentyOddRange)
  println(twentyOddFor)
  println(twentyOddFilter)
  /*

      2. Write a function titled “factors” that takes a number and returns a list of its factors, other than 1 and
      the number itself. For example, factors(15) should return List(3, 5). Then write a new function that applies
      “factors” to a list of numbers. Try using the list of Long numbers you generated in exercise 1. For example,
      executing this function with List(9, 11, 13, 15) should return List(3, 3, 5), because the factor of 9 is 3
      while the factors of 15 are 3 again and 5. Is this a good place to use map and flatten? Or would a for-loop be
      a better fit?
*/
  @scala.annotation.tailrec
  def factors(x: Long, c: Long = 2, l: Seq[Long] = Nil): Seq[Long] =
    if (c == x) l
      else if (x % c != 0) factors(x, c+1, l)
      else factors(x, c+1, l :+ c)


  val fact12 = factors(12)
  //val factorsOfOdd = twentyOddFilter flatMap(i => factors(i))
  //println(factorsOfOdd)
  def uniqueFactors(l: Seq[Long]) = l.filter(_ != 1).flatMap(i => factors(i))
  println(uniqueFactors(twentyOddFilter))
  /*
    3. Write a function, first[A](items: List[A], count: Int): List[A], that returns the first x number of items in a
     given list. For example, first(List('a','t','o'), 2) should return List('a','t'). You could make this a
     one-liner by invoking one of the built-in list operations that already performs this task, or (preferably)
     implement your own solution. Can you do so with a for-loop? With foldLeft? With a recursive function that only
     accesses head and tail?
  */
  def first[A](items: List[A], count: Int): List[A] = {
    @scala.annotation.tailrec
    def loop(i: List[A], count: Int, r: List[A] = Nil): List[A] =
      if (count == 0) r
      else loop(i.tail, count-1, r :+ i.head)
    loop(items, count)
  }
  println(first(twentyOddFilter, 3))

  def firstOneLiner[A](items: List[A], count: Int): List[A] = items.take(count)

  println(firstOneLiner(twentyOddFilter, 3))

  /*
    4. Write a function that takes a list of strings and returns the longest string in the list. Can you avoid using
    mutable variables here? This is an excellent candidate for the list-folding operations (Table 6-5) we studied.
    Can you implement this with both fold and reduce? Would your function be more useful if it took a function
    parameter that compared two strings and returned the preferred one? How about if this function was applicable to
    generic lists, i.e., lists of any type?
  */
  val longString: String = "Write a function that takes a list of strings and returns the longest string in the list"
  val stringList: List[String] = longString.split(" ").toList
  println(stringList)
  def longest(a: List[String]): String = a.maxBy(_.length)
  def longReduce(a: List[String]): String = a.reduce((a,b) => if (a.length > b.length)a else b)
  def polyReduce[A](a: List[A])(f: (A, A) => A):A = a.reduce(f)
    println(longest(stringList))
    println(longReduce(stringList))
    println(polyReduce(stringList)((a,b) => if (a.size > b.size)a else b))
    println(polyReduce(twentyOddFilter)((a,b) => if (a > b)a else b) )
  /*
    5. Write a function that reverses a list. Can you write this as a recursive function? This may be a good place
    for a match expression.
  */
    def reverseList[A](a: List[A]): List[A] = {
      @scala.annotation.tailrec
      def loop(as: List[A], acc: List[A] = Nil): List[A] = {
        if (as.isEmpty) acc
        else loop(as.tail, as.head :: acc)
      }
      loop(a)
    }
  println(reverseList(twentyOddFilter))
  println(reverseList(stringList))
  /*
    6. Write a function that takes a List[String] and returns a (List[String],List[String]), a tuple of string lists.
     The first list should be items in the original list that are palindromes (written the same forward and backward,
      like “racecar”). The second list in the tuple should be all of the remaining items from the original list. You
      can implement this easily with partition, but are there other operations you could use instead?
  */
  val palindromeDirty: String = " a aa aba abba ada aga aha aja aka ala alla ama ana anna ara ata aua aya\nb bab beb " +
    "begeb beheb beleb bgb bob bub\nccc cic cnc\nd dad dvd\ne ebbe ecce effe egge ehe eie elle esse ewe exe\ng gag gig\nh hgh huh höh hüh\nimi\nk kaak kajak kak kasak kelek kmk kok ksk\nlegel level lol\nmadam malajalam malayalam mam maqam mem mgm mhm mim minim mm monom mum\nn neben neffen nellen nennen neppen netten netzten neuen non nun\no obo odo oho otto\npeep pep poop pop pup\nradar rar redder reger reittier reliefpfeiler renner rentner retter rotor rör\nsagas sees seles sennes sereneres sexes sis sms sokos solos ss staats stets succus sucus\nt tannat tartrat tat teet tot tut tät töt tööt tüt\nu uhu ulu upu uru\nv\nw wow www\nz"
  val palindromeClean: String = palindromeDirty.replace("\n", " ")
  val exerciseStringDirty: String = "Write a function that takes a List[String] and returns a (List[String]," +
    "List[String])" +
    ", a tuple of string lists.\n     The first list should be items in the original list that are palindromes (written the same forward and backward,\n      like “racecar”). The second list in the tuple should be all of the remaining items from the original list. You\n      can implement this easily with partition, but are there other operations you could use instead?"
  val exerciseString: String = exerciseStringDirty.replace("\n", " ").replaceAll("[^A-Za-z0-9]+", " ")

  val wordList: List[String] = exerciseString.split(" ").toList
  val paliList: List[String] = palindromeClean.split(" ").toList
  val mixedTuple = wordList.zip(paliList)
  val mixedList: List[String] = mixedTuple.flatten{case (a,b) => List(a,b)}
  //println(mixedList)


  def paliTeiler(l: List[String]):(List[String], List[String]) = {
    //val rs: (List[String], List[String]) = (Nil, Nil)
    @scala.annotation.tailrec
    def loop(ls: List[String], acc:(List[String], List[String]) = (Nil, Nil)): (List[String], List[String])=
      if (ls.isEmpty) acc
      else {
        if(ls.head == ls.head.reverse) loop(ls.tail, (ls.head :: acc._1, acc._2))
        else  loop(ls.tail, (acc._1, ls.head :: acc._2))
      }
    loop(l)
    }
  println(paliTeiler(mixedList)._1)

  /*
      7. The last exercise in this chapter is a multipart problem. We’ll be reading and processing a forecast from
      the excellent and free OpenWeatherMap API. To read content from the URL we’ll use the Scala library operation
      io.Source.+fromURL(url: String), which returns an +io.Source instance. Then we’ll reduce the source to a
      collection of individual lines using the getLines.toList operation. Here is an example of using io.Source to
      read content from a URL, separate it into lines, and return the result as a list of strings:
        scala> val l: List[String] = io.Source.fromURL(url).getLines.toList
      Here is the URL we will use to retrieve the weather
      forecast, in XML format:
        scala> val url =   "http://api.openweathermap.org/data/2.5/forecast?mode=xml&lat=55&lon=0"
      Go ahead and read this URL into a list of strings. Once you have
      it, print out the first line to verify you’ve captured an XML file. The result should look pretty much like
      this: scala> println( l(0) ) <?xml version="1.0" encoding="utf-8"?> If you don’t see an XML header, make sure
      that your URL is correct and your Internet connection is up. Let’s begin working with this List[String]
      containing the XML document.
  */
    val source = scala.io.Source.fromFile("/Users/joecz/Documents/openWeather")
    val key: String = source.mkString
    source.close()
    val city = "Berlin"
    val days = 16

    val url: String = s"https://api.openweathermap.org/data/2.5/weather?q=$city&mode=xml&units=metric&appid=$key"

    //val weatherGetter: String = scala.io.Source.fromURL(url).mkString
    //val writer = new PrintWriter(new File("weather.xml"))
    //writer.write(weatherGetter)
    //writer.close()
    val weather = Source.fromFile("weather.xml").getLines().toList
    source.close()
    weather.foreach{x=>println(x)}
  /*
        1. To make doubly sure we have the right content, print out the top 10 lines of the file. This should be a
        one-liner.
  */
    //weather.take(10).foreach{x=>println(x)}
  /*
        2. The forecast’s city’s name is there in the first 10 lines. Grab it from the correct line and print out its
         XML element. Then extract the city name and country code from their XML elements and print them out together
          (e.g., “Paris, FR”). This is a good place to use regular expressions to extract the text from XML tags (see
           Regular expressions). If you don’t want to use regular expression capturing groups, you could instead use
           the replaceAll() operation on strings to remove the text surrounding the city name and country name.
  */
    // function to find and print xml element with string
    def findElement(s: String, l: List[String]): String = {
      @scala.annotation.tailrec
      def loop(s: String, l: List[String], acc: String = ""): String = {
        if (l.isEmpty) acc
        else if (l.head.contains(s)) loop(s, l.tail, acc + l.head)
        else loop(s, l.tail, acc)
      }
      loop(s, l)
    }
    println(findElement("city", weather))
    def parseXmlStringToList(s:String): List[String] = {
      s.replaceAll("[\"=<>]", " ").trim.replaceAll(" +", " ").split(" ").toList
    }
    val cityInfo = parseXmlStringToList(findElement("city", weather))
    println(cityInfo)
    println(cityInfo(cityInfo.length - 2))

        //3. How many forecast segments are there? What is the shortest expression you can write to count the segments?
    val forecastSegments = weather.length - 9 // minus one for the xml bla bla
    println(forecastSegments)

  /*
        4. The “symbol” XML element in each forecast segment includes a description of the weather forecast. Extract
        this element in the same way you extracted the city name and country code. Try iterating through the
        forecasts, printing out the description. Then create an informal weather report by printing out the weather
        descriptions over the next 12 hours (not including the XML elements).
  */
    val forecastInfo = parseXmlStringToList(findElement("temperature", weather))
    println(forecastInfo)
    val tempString = s"Today's average temperature is ${forecastInfo(2)} degrees ${forecastInfo(8)}, with a max of " +
      s"${forecastInfo(6)} ${forecastInfo(8)} and a low of ${forecastInfo(4)} ${forecastInfo(8)}"
    println(tempString)
  /*
        5. Let’s find out what descriptions are used in this forecast. Print a sorted listing of all of these
        descriptions in the forecast, with duplicate entries removed.
  */
    def getLeadNamesFromXMLList(l: List[String]): Set[String] = {
      @scala.annotation.tailrec
      def loop(ls: List[String], acc: scala.collection.mutable.Set[String]): Set[String] =
        if (ls.isEmpty) acc.toSet
        else {
          val backslash = "[/?]".r
          val returnString = parseXmlStringToList(ls.head)(0)
          val regexResult = backslash.findFirstIn(returnString)
          if (regexResult.isEmpty) loop(ls.tail, acc += returnString)
          else loop(ls.tail, acc)
        }
        val acc = scala.collection.mutable.Set[String]()
        loop(l, acc)
    }
    val leadNames = getLeadNamesFromXMLList(weather)
    print(leadNames)
  /*
        6. These descriptions may be useful later. Included in the “symbol” XML element is an attribute containing
        the symbol number. Create a Map from the symbol number to the description. Verify this is accurate by
        manually accessing symbol values from the forecast and checking that the description matches the XML document.

        7. What are the high and low temperatures over the next 24 hours?

        8. What is the average temperature in this weather forecast? You can use the “value” attribute in the
        temperature element to calculate this value.

Jason Swartz. Learning Scala (Kindle Locations 2477-2478). O’Reilly Media. Kindle Edition.
   */
}
