::#!
@echo off
call scala %0 %*
goto :eof
::!#
println("Hello, Welcome to Scala Script.....!!!!!")

def greet(name: String): String = s"Hello, $name!"


// Entry point for our script
args.toList match {
  case List(name) => {
    val greeting = greet(name)
    println(greeting)
  }
  case _ =>
    println("usage: HelloScript.scala <name>")
}
