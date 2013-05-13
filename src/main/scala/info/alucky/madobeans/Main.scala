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
      case Some(ast) => try{
        Exec exec(ast, env)
      }catch{
        case err: Throwable => println("error : " + err.getClass.getName() + ":" + err.getMessage())
      }
      case None => println("Error")
    }
  } else {
    println("Hello!\n")
    var src = ""
    breakable {
      do{
        src = {print("(MadoBeans)> ");readLine}
        if(src == ":exit") break
        val ast = parser.parse(src)
        ast match {
          case Some(ast) => {
            try{
              val v = Exec exec(ast, env)
              println(" => " + v)
            }catch{
              case err:Throwable => println("error : " + err.getClass.getName() + ":" + err.getMessage())
            }
          }
          case None => println("Error")
        }
      }while(true)
    }
    println("See you!")
  }
}