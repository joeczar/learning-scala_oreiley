import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object Futures extends App {
  println("gangsta!")

  def nextFtr(i: Int = 0) = Future {
    def rand(x: Int) = Random.nextInt(x)

    Thread.sleep(rand(5000))
    if (rand(3) > 0) (i + 1) else throw new Exception("╭∩╮(Ο_Ο)╭∩╮")
  }
  val justFtr = nextFtr(1)
  val fallbackFtr = nextFtr(1) fallbackTo nextFtr(2)
  val flatMapFtr = nextFtr(1).flatMap(x => nextFtr(x))
  val mapFtr = nextFtr(1) map (_ * 2)
  val onCompleteFtr = nextFtr() onComplete { _ getOrElse 0 }
  //val onFailureFtr = nextFtr() onFailure { case _ => "╭∩╮( °͜ʖ͡° )" }
  //val onSuccesFtr = nextFtr() onSuccess { case x => s"Got $x" }
  val sequenceFtr = concurrent.Future sequence List(nextFtr(1), nextFtr(5))

}
