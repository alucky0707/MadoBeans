package info.alucky.madobeans

object FizzBuzz extends App {
  val parser = new Parser
  val str = """
    for := {i, to, iter ->
      i <= to ? (
        iter(i)
        for(i+1,to,iter)
      ) : 0
    }
    
    for(1,100,{i ->
      fizz := i % 3 === 0
      buzz := i % 5 === 0
      println(fizz && buzz ? "FizzBuzz" :
                      fizz ? "Fizz"     :
                      buzz ? "Buzz"     : i)
    })
  """
  object PrintlnFunc extends Func {
    def apply(vargs: List[Any]) = println(vargs.mkString(","))
  }
  val ast = parser.parse(str)
  val env = new Env(None)
  env.set("println",PrintlnFunc)
  println(ast)
  ast match {
    case Some(ast) => println(Exec.exec(ast, env))
    case None => println("は？")
  }
}