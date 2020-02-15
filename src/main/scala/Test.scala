object Test extends App {
  def safeStringOp(s: String, f: String => String) = {
    if (s != null) f(s) else s
  }

  val uuid = java.util.UUID.randomUUID.toString
  val uuid_2 = java.util.UUID.randomUUID.toString
  val timedUUID = safeStringOp(uuid, { s =>
    val now = System.currentTimeMillis
    val timed = s.take(24) + now
    timed.toUpperCase
  })
  println("1", timedUUID)

  def safeStringOp2(s: String)(f: String => String) = {
    if (s != null) f(s) else s
  }
  val timedUUID_2 = safeStringOp2(uuid_2) { s =>
    val now = System.currentTimeMillis
    val timed = s.take(24) + now
    timed.toUpperCase
  }
  println("2", timedUUID_2)

  def timer[A](f: => A): A = {
    def now = System.currentTimeMillis
    val start = now; val a = f; val end = now
    println(s"Executed in ${end - start} ms")
    a
  }
  val veryRandomAmount = timer {
    util.Random.setSeed(System.currentTimeMillis)
    for (i <- 1 to 100000) util.Random.nextDouble
    util.Random.nextDouble
  }
  println(veryRandomAmount)
}
