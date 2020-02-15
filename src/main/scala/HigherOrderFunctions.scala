object HigherOrderFunctions extends App {
/*
Excercises:
  1. Write a function literal that takes two integers and returns the higher number.
  Then write a higher-order function that takes a 3-sized tuple of integers plus this function literal,
  and uses it to return the maximum value in the tuple.
*/
  val higher = (a: Int, b: Int) => if (a>b) a else b

  def highest(t: (Int, Int, Int))(f: (Int,Int) => Int): Int = {
    f(t._3, f(t._1, t._2))
  }

  println(highest(3, 43, 1)(higher))
  /*
  2. The library function util.Random.nextInt returns a random integer. Use it to invoke the “max” function with two
  random integers plus a function that returns the larger of two given integers. Do the same with a function that
  returns the smaller of two given integers, and then a function that returns the second integer every time.
*/
  def randomInt =util.Random.nextInt

  val t3 = (randomInt, randomInt, randomInt)

  println("higher ",higher(randomInt, randomInt))

  println("three random ",highest(t3)(higher))

  /*
  3. Write a higher-order function that takes an integer and returns a function. The returned function should take a
  single integer argument (say, “x”) and return the product of x and the integer passed to the higher-order function.
*/
  def addTheSecond(a: Int): (Int) => Int = {
    def theSecond(b: Int): Int = {
      a + b
    }
    theSecond
  }
  val num2 = addTheSecond(3)
  println(num2(4))
  // their solution
  def multy(x: Int) = (y: Int) => x * y
  val tripler = multy(3)
  println(tripler(222))

  /*
  4. Let’s say that you happened to run across this function while reviewing another developer’s code:
      def fzero[A](x: A)(f: A => Unit): A = { f(x); x }
     What does this function accomplish? Can you give an example of how you might invoke it?
  */
  def fzero[A](x: A)(f: A => Unit): A = { f(x); x }

  fzero("eat me")(f => println(f.replace(" ", "-")))

  /*

  5. There’s a function named “square” that you would like to store in a function value. Is this the right way
   to do it? How else can you store a function in a value?
      def square(m: Double) = m * m val sq = square
  */
  val square = (m: Double) => m * m
  val sq = square
  // What they wanted
  def square(m: Double) = m * m
  // val sq = square returns this error:  error: missing argument list for method square
  //Unapplied methods are only converted to functions when a function type is expected.
  //You can make this conversion explicit by writing `square _` or `square(_)` instead of `square`.
  //       val sq = square
  //                ^
  val sq2 = square _

  /*

  6. Write a function called “conditional” that takes a value x and two functions, p and f, and returns a value of
  the same type as x. The p function is a predicate, taking the value x and returning a Boolean b. The f function also
  takes the value x and returns a new value of the same type. Your “conditional” function should only invoke the
  function f(x) if p(x) is true, and otherwise return x. How many type parameters will the “conditional”
  function require?

  */
    def conditional[A](x: A, p: A => Boolean, f: A => A): A = {
      if (p(x)) f(x) else x
    }
    val a = conditional[String]("yo", _.size > 4, _.reverse)
    println(a)
  /*

  7. Do you recall the “typesafe” challenge from the exercises in Chapter 3? There is a popular coding interview
  question I’ll call “typesafe,” in which the numbers 1-100 must be printed one per line. The catch is that multiples
  of 3 must replace the number with the word “type,” while multiples of 5 must replace the number with the word “safe.”
  Of course, multiples of 15 must print “typesafe.” Use the “conditional” function from exercise 6 to implement this
  challenge. Would your solution be shorter if the return type of “conditional” did not match the type of the
  parameter x? Experiment with an altered version of the “conditional” function that works better with this challenge.
 */
  for(i <- 1 to 100) {
    val a1 = conditional[Int](i, _ % 3 == 0, x => { print("type") ; 0})
    val a2 = conditional[Int](i, _ % 5 == 0, x => { print("safe") ; 0})
    if (a1 > 0 && a2 > 0) print(i)
    println("")
  }
   def myTypeSafe(n: Int): String = {
    @scala.annotation.tailrec
    def loop(x: Int = 0, acc: String = ""): String = {
      if (x == 0) acc
      else {
        var output = ""
        if (x % 3 == 0 && x % 5 == 0) output += "TypeSafe"
        else if (x % 3 == 0) output += "Type"
        else if (x % 5 == 0) output += "Safe"
        else x.toString
        loop(x - 1, s"$x $output \n" +acc)
      }
      }
     loop(n)
    }
   println(myTypeSafe(100))

  def conditional2[A](x: A, p: A => Boolean, f: A => String): String = {
    if (p(x)) f(x) else ""
  }
  for (i <- 1 to 100) {
    val a1 = conditional2[Int](i, _ % 3 == 0, _ => "Type")
    val a2 = conditional2[Int](i, _ % 5 == 0, _ => "Safe")
    //val a3 = conditional2[Int](i, _ % 3 > 0 && i % 5 > 0, x => s"# $x")
    println(s"$i " + a1 + a2)
  }
}
