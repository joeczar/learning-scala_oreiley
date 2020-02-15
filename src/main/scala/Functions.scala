import scala.annotation.tailrec

object Functions extends App {
  /*
  Exercises
Write a function that computes the area of a circle given its radius.
Provide an alternate form of the function in exercise 1 that takes the radius as a String. What happens if your function is invoked with an empty String ?
Write a recursive function that prints the values from 5 to 50 by fives, without using for or while loops. Can you make it tail-recursive?
Write a function that takes a milliseconds value and returns a string describing the value in days, hours, minutes, and seconds. What’s the optimal type for the input value?
Write a function that calculates the first value raised to the exponent of the second value. Try writing this first using math.pow, then with your own calculation. Did you implement it with variables? Is there a solution available that only uses immutable data? Did you choose a numeric type that is large enough for your uses?
Write a function that calculates the difference between a pair of 2D points (x and y) and returns the result as a point. Hint: this would be a good use for tuples (see Tuples).
Write a function that takes a 2-sized tuple and returns it with the Int value (if included) in the first position. Hint: this would be a good use for type parameters and the isInstanceOf type operation.
Write a function that takes a 3-sized tuple and returns a 6-sized tuple, with each original parameter followed by its String representation. For example, invoking the function with (true, 22.25, "yes") should return (true, "true", 22.5, "22.5", "yes", "yes"). Can you ensure that tuples of all possible types are compatible with your function? When you invoke this function, can you do so with explicit types not only in the function result but in the value that you use to store the result?
   */
  def circleArea(r: Double): Double = {
    Math.PI * Math.pow(r,2.0)
  }
  print(circleArea(6.0))
  def circleArea2(r: String): Double = {
    if (r.length == 0) sys.error("That's not a valid radius")
    val radius = r.toDouble
    circleArea(radius)
  }
  println(circleArea2("6"))
  @tailrec
  def printBy(s: Int, e: Int, i: Int): Unit = {
    if (s<=e) {
      println(s)
      printBy(s+i, e, i)
    }
  }
  printBy(5, 50, 5)

  def milliSecsToDays(millis: Long): String = {

    val times = Array(
      ("millisecond", 1L),
      ("second", 1000L),
      ("minute" ,1000 * 60L),
      ("hour", 1000 * 60 * 60L),
      ("day", 1000 * 60 * 60 * 24L),
      ("month", 2629746000L),
      ("year",31556952000L),
      ("decade", 315576000000L),
      ("centurie", 3155760000000L)
    )

    var results = new Array[Long](times.length)

    @scala.annotation.tailrec
    def millisToDays(millis: Long, arr: Array[(String, Long)], count: Int ): Array[Long] = {
      val timeVal = arr(count)._2

      if (count <= 0) {
        results(0) = millis
        results
      }
      else {
        val rest = millis % timeVal
        results(count) = millis / timeVal
        millisToDays(rest, arr, count - 1)
      }
    }
    val count = times.length
    results = millisToDays(millis, times, count-1)

    @scala.annotation.tailrec
    def returnString(results: Array[Long], times: Array[(String, Long)], count: Int = results.length, acc: String = ""): String = {
      if (count < 0) acc
      else if (results(count) == 0) returnString(results, times, count-1, acc)
      else
        returnString(results, times,  count-1, acc + s"${results(count)} ${times(count)._1}s, ")
    }
    returnString(results, times, count-1)
  }
  val threeDays = milliSecsToDays(6666666666666L)
  println(threeDays)

  @scala.annotation.tailrec
  def toThePower(a: Int, b: Int, res: Int = 1): Int = {
    if(b == 1) return a * res
    else
      toThePower(a, b-1, a*res)
  }
  println(toThePower(2, 8))
  /*
    Write a function that calculates the difference between a pair of 2D points (x and y)
    and returns the result as a point. Hint: this would be a good use for tuples (see Tuples).
    (x, y) - (x2, y2) = (xd, yd)? or dist = sqrt((x2-x1)^2 + (y2-y1)^2)
   */
  def pointDifference(p1: (Int, Int), p2: (Int, Int)): (Double, (Double, Double)) = {
    val distance: Double = Math.sqrt(Math.pow((p2._1 - p1._1), 2) + Math.pow((p2._1 - p1._1), 2))
    val difference:(Double, Double) = ( p2._1 - p1._1, p2._2 - p1._2)
    (distance,(difference))
  }
  val pointDiffResult = pointDifference((4, 9), (122, 27))
  println(pointDiffResult)

  /*
  Write a function that takes a 2-sized tuple and returns it with the Int value (if included) in the first position.
  Hint: this would be a good use for type parameters and the isInstanceOf type operation.
    f(("sec", 3123)) = (3123, "sec")
    I see an anonymous class/trait setup here as there is no way of allowing tuples with other types through.
    That springs the chapter though.
   */
  def tupleIntSorter(t: (Int, String)):(String, Int) = {
      val switched = (t._2, t._1)
      switched
  }
  // had to look this guy up :( good news is, it's not good practice because it's not type safe
  def intFirst[A, B](t: (A, B)): (Any, Any) = {
    def isInt(x: Any) = x.isInstanceOf[Int] // Cool way to shorten this little guy
    (isInt(t._1), isInt(t._2)) match {
      case (false, true) => (t._2, t._1)
      case _ => t
    }
  }
  //val noGO = tupleIntSorter(("3", 3))
  println(intFirst("666!", 3))

  /*
  Write a function that takes a 3-sized tuple and returns a 6-sized tuple, with each original parameter
  followed by its String representation. For example, invoking the function with (true, 22.25, "yes")
  should return (true, "true", 22.5, "22.5", "yes", "yes"). Can you ensure that tuples of all possible
  types are compatible with your function? When you invoke this function, can you do so with explicit
  types not only in the function result but in the value that you use to store the result?
   */

  def sixTupleIt[A, B, C](t: (A, B, C)): (A, String, B, String, C, String) = {
    (t._1, t._1.toString, t._2, t._2.toString, t._3, t._3.toString)
  }
  val sixTupleted = sixTupleIt(1, "Hot", false)
  println(sixTupleted)


}
