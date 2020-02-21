import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object WeatherFuture extends App {

  val source = scala.io.Source.fromFile("/Users/joecz/Documents/openWeather")
  val key = source.mkString
  source.close()

  def cityTemp(name: String): Double = {
    val url = s"https://api.openweathermap.org/data/2.5/forecast?q=$name&appid=$key"
    val json = scala.io.Source.fromURL(url).mkString.trim
    val pattern = """.*"temp":([\d.]+).*""".r
    val pattern(temp) = json
    temp.toDouble
  }
  println(cityTemp("Berlin"), cityTemp("Fresno"), cityTemp("London"))


  val cityTemps = Future.sequence(Seq(
    Future(cityTemp("Fresno")), Future(cityTemp("London")))
  )

  cityTemps onComplete {
    case Seq(x, y) => println(s" $x K, $y K ")
    //case Seq(x,y) if (y > x) => println(s"London is warmer $y K") //onSuccess is no longer in use and this wont even compile
    case Success(value) => value.foreach(x => println(s"from map $x"))
    // foreach and map are the only ones that return a value
    case Failure(exception) => throw exception
    case _ => println(""" ¯\_(ツ)_/¯ """)

  }

}
