import scala.annotation.tailrec

object MoreCollectionsExercises extends App {
/*
    1. Write a function that returns a list of the first x elements in the Fibonacci series Can you write
    this with a Buffer? Would a Builder be appropriate here?
 */

    def fiboList(n: Int): List[Int] = {
      @scala.annotation.tailrec
      def fiboHelper(x: Int, prev: Int = 0, next: Int = 1, acc: List[Int] = List()): List[Int] = {
          x match {
            case x if x == acc.length => acc
              case 0 => acc :+ prev
              case 1 => acc :+ next :+ prev
              case _ => fiboHelper(x , next, next + prev, acc :+ (next + prev))
           }
      }
        fiboHelper(n)
    }

    val fiboList8 = fiboList(8)
    println(fiboList8)
/*
    2. Write a new Fibonacci function that adds new Fibonacci numbers to an existing list of numbers. It
    should take a list of numbers (List[Int]) and the count of new elements to add and return a new list
    (List[Int]). Although the input list and returned lists are immutable, you should be able to use a
    mutable list inside your function. Can you also write this function using only immutable lists?
    Which version, using mutable versus immutable collections, is more appropriate and readable?
*/
  def isFibonacciList(l: List[Int]): Boolean = {
    @scala.annotation.tailrec
    def loop(ls: List[Int], prev: Int = 0, next: Int = 0, bool: Boolean = false): Boolean = {
      if (ls.isEmpty) bool
      else {
        if (ls.head == 1 && ls.tail.head == 2) loop(ls.tail, ls.head, ls.tail.head, true)
        else if (ls.length > 3 && ls.head == prev + next) loop(ls.tail, ls.head, ls.tail.head, true)
        else ls.head + ls.tail.head == ls.tail.tail.head
      }
    }
    loop(l)
  }

  println(isFibonacciList(fiboList(20)))
  println(isFibonacciList(List(1,2,3,4,5,6)))


  def fibExtender(l: List[Int], count: Int): List[Int] = {
     //check if list is a fibonacci list
     @scala.annotation.tailrec
     def fiboLoop(prev: Int, next: Int, count: Int, fibList: List[Int]):List[Int] = {
       if (count <= 0) fibList
       else fiboLoop(next, next + prev, count - 1, fibList :+ (next + prev))
     }
    if (!isFibonacciList(l)) throw new Exception("Not a Fibonacci list")
    else {
      val reversed = l.reverse
      fiboLoop(reversed.tail.head, reversed.head, count, l)
    }
  }

  println(fibExtender(fiboList8, 8))

/*
    3. The Stream collection is a great solution for creating a Fibonacci series. Create a stream that
    will generate a Fibonacci series. Use it to print out the first 100 elements in the series, in a
    formatted report of 10 comma-delimited elements per line.
*/
def fiboLazy(n: BigInt): LazyList[BigInt] = {
  @scala.annotation.tailrec
  def fiboHelper(x: BigInt, prev: BigInt = 0, next: BigInt = 1, acc: LazyList[BigInt] = LazyList()): LazyList[BigInt] = {
    val bigZero: BigInt = 0
    val bigOne: BigInt = 1
    x match {
      case x if x == acc.length => acc
      case x if x == bigZero => acc :+ prev
      case x if x == bigOne => acc :+ next :+ prev
      case _ => fiboHelper(x , next, next + prev, acc :+ (next + prev))
    }
  }
  fiboHelper(n)
}
  def fiboFormat(ll: LazyList[BigInt]): Unit = {
    var count = 0
    ll.foreach{ x =>

      if (count > 1 && count % 10 == 0) {
        print(s"$x, \n")
        count += 1
      }
      else {
        count += 1
        print(s"$x, ")
      }
    }
  }
  println(fiboLazy(10))
  fiboFormat(fiboLazy(100))

/*
    4. Write a function that takes an element in the Fibonacci series and returns the following element
    in the series. For example, fibNext(8) should return 13. How will you handle invalid input such as
    fixNext(9)? What are your options for conveying the lack of a return value to callers?
*/
  def isFibonacci(n: BigInt): Boolean = {
      @scala.annotation.tailrec
      def fiboHelper(x: BigInt, prev: BigInt = 0, next: BigInt = 1, acc: BigInt = 0): Boolean = {
        val bigZero: BigInt = 0
        val bigOne: BigInt = 1
        x match {
          case x if x == acc => true
          case x if x < acc => false
          case x if x == bigZero => true
          case x if x == bigOne => true
          case _ => fiboHelper(x, next, next + prev, (next + prev))
        }
      }
      fiboHelper(n)
    }
  println(isFibonacci(6))
  println(isFibonacci(8))
  println(isFibonacci(BigInt(57314784)))
  println(isFibonacci(BigInt(1836311903)))

  def fiboNext(n: BigInt): BigInt = {
    @scala.annotation.tailrec
    def fiboHelper(x: BigInt, prev: BigInt = 0, next: BigInt = 1, acc: BigInt = 0): BigInt = {
      val bigZero: BigInt = 0
      val bigOne: BigInt = 1
      x match {
        case x if x == acc =>  next + prev
        case x if x < acc => throw new Exception("Not a Fibonacci number")
        case x if x == bigZero => 1
        case x if x == bigOne => 2
        case _ => fiboHelper(x, next, next + prev, (next + prev))
      }
    }
    fiboHelper(n)
  }
  //println(fiboNext(9))
  println(fiboNext(BigInt(1836311903)))
/*
  2. In the example
  for Array collections (see Arrays) we used the java.io.File(<path>).listFiles operation to return an
  array of files in the current directory. Write a function that does the same thing for a directory,
  and converts each entry into its String representation using the toString method. Filter out any
  dot-files (files that begin with the . character)and print the rest of the files separated by a
  semicolon (;). Test this out in a directory on your computer that has a significant number of files.
 */

  val files = new java.io.File("""\Users\joecz\OneDrive""").listFiles
  val pattern = """/^\./""".r
  val pattern2 = "."
  val cleanedFiles = files.map(x => x.toString.replace("\\Users\\joecz\\OneDrive\\", ""))
  val filteredFiles = cleanedFiles.filter(_ != pattern)
  println(files.length)
  filteredFiles.foreach(x => print( x + "; "))

def listFiles(path: String): Vector[String] = {
  val files = new java.io.File(path).listFiles().toVector
  files.map(x => x.toString.replace(path, ""))
  }

  @tailrec
  def fileNamesToString(v: Vector[String], acc: String = ""): String = {
    if (v.isEmpty) acc
    else fileNamesToString(v.tail, acc + v.head + "; ")
  }
  val downloads = listFiles("\\Users\\joecz\\Downloads\\")
  val filteredDownloads = listFiles("\\Users\\joecz\\Downloads\\").filterNot(_ startsWith("."))
  println("\n\n" )
  println(downloads)
  println("\n\n" )
  println(fileNamesToString(filteredDownloads))

  /*
  Take the file listing from exercise 2 and print a report showing each letter in the alphabet followed by the number
  of files that start with that letter.

  I had to look this one up :(
   */
  println("\n\n ############" )
  val groupedByName = filteredDownloads.groupBy(_.head.toLower).toList.sortBy(_._1) //.filter(x => x._1.isLetter)
  println(groupedByName)
  println("\n\n" )
  for { (c,l) <- groupedByName } { println(s"$c has ${l.size} files") }
/*
    Write a function to return the product of two numbers that are each specified as a String, not a numeric type.
    Will you support both integers and floating-point numbers? How will you convey if either or both of the inputs
    are invalid? Can you handle the converted numbers using a match expression? How about with a for-loop?
 */
def productFromStrings(int1: String, int2: String): Int = {
  val stringList = List(int1, int2)
  val parsed = for { num <- stringList } yield {
    try {
      Some(Integer.parseInt(num.trim))
    } catch {
      case e: NumberFormatException => None
    }
  }
  val noOption: List[Int] = for { num <- parsed } yield {
    num match {
      case Some(i) => i
      case None =>  println("That is not a valid Int"); 0
    }
  }
  noOption(0) * noOption(1)
}
  println(productFromStrings("123", "456"))

  def wrapJVMCalls(call: String): String = {
    val called: Option[String] = try {
      Option(System.getProperty(call))
    } catch {  case e: Exception => Option(s"$e  ¯\\_(ツ)_/¯  ") }
    called match {
      case Some(i) => i.toString
      case None => "None"
    }
  }
  println(wrapJVMCalls(""))

  val starBreak = "\n" + ("*"*100) + "\n"
  println(starBreak)

  println("GitHub RssReader ")


}
