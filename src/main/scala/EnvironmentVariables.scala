
import scala.jdk.CollectionConverters._

object EnvironmentVariables extends App {

  val environmentVars = System.getenv().asScala
  for ((k,v) <- environmentVars) println(s"ENVVARS --- key: $k, value: $v")

  val properties = System.getProperties.asScala
  for((k,v) <- properties) println(s"PROPS --- key: $k, value: $v")
}
