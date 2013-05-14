package info.alucky.madobeans

import scala.io._
import scala.util.control.Breaks.{break, breakable}
import javax.swing.JOptionPane

object Main extends App {
  val parser = new Parser
  val env = new Env(None)
  env.set("println",new Func(){
    def apply(vargs: List[Any]) = println(vargs.mkString(" "))
  })
  env.set("alert",new Func(){
    def apply(vargs: List[Any]) = JOptionPane.showMessageDialog(null, vargs.mkString(" "))
  })
  if(args.length >= 2) {
    val file = Source fromFile(args(1), "UTF-8")
    val src = file.getLines mkString("\n")
    val ast = parser.parse(src)
    ast match {
<<<<<<< HEAD
      case Some(ast) => try{
        Exec exec(ast, env)
      }catch{
        case err: Throwable => println("error : " + err.getClass.getName() + ":" + err.getMessage())
      }
      case None => println("Parse Error")
=======
      case Some(ast) => try{Exec exec(ast, env)}catch{ case err: IllegalStateException => println(err.getMessage()) }
      case None => println("Error")
>>>>>>> parent of f899fb7... エラーメッセージを心なし親切にした。
    }
  } else {
    println("Mado Language Beans(Tofu) 0.1.1\n")
    var src = ""
    breakable {
      do{
        src = {print("(MadoBeans)> ");readLine}
        if(src.trim == ":exit") break
        val ast = parser.parse(src)
        ast match {
          case Some(ast) => {
            val v = Exec exec(ast, env)
            println(" => " + v)
          }
          case None => println("Parse Error")
        }
      }while(true)
    }
  }
}