package info.alucky.madobeans

import scala.collection.mutable._;

class Env(val parent: Option[Env]) {
  val env = new HashMap[String,Any]
  def get(key: String): Option[Any] = env.get(key) match {
    case Some(v) => Some(v)
    case None => parent match {
      case Some(p) => p.get(key)
      case None => None
    }
  }
  def set(key: String, v: Any) = {env += key->v}
  def has(key: String): Boolean = env.get(key) match {
    case Some(_) => true
    case None => parent match {
      case Some(p) => p.has(key)
      case None => false
    }
  }
  def defined(key: String) = env.get(key) match {
    case Some(_) => true
    case None => false
  }
  def update(key: String, v:Any): Unit = if(defined(key)){
    env(key) = v
  }else{
    parent match {
      case Some(p) => p(key) = v
      case None => throw new IllegalStateException(key + " is not found")
    }
  }
}

trait Func {
  def apply(vargs: List[Any]): Any
}

case class FuncVal(var args: List[AST],expr: AST,env: Env) extends Func {
  args = if(args.length == 1 && args(0).isInstanceOf[AST.Empty]) List() else args
  def apply(vargs: List[Any]) = {
    val scope = new Env(Some(env))
    args.zip(vargs).foreach {
      case (AST.WordLit(s),v) => scope.set(s,v)
    }
    Exec.exec(expr,scope)
  }
}

object Exec {
  import AST._
  def exec(ast: AST,env: Env): Any = ast match {
    case StmtList(lines) => lines.fold(0.asInstanceOf[Any]){(a,b)=> b match {
      case Empty() => a
      case _ => exec(b.asInstanceOf[AST],env)
    }
    }
    case DefineStmt(WordLit(word),expr) => {
        if(env.defined(word)) throw new IllegalStateException(word + " has already defined")
        env.set(word,exec(expr,env))
        ()
    }
    case BinOp("=",a,b) => a match {
    case WordLit(s) => if(env.has(s)){
        val v = exec(b,env)
                env(s) = v
                v
    }else{
        throw new IllegalStateException(s + " is not defined")
    }
    }
    case BinOp(op,a,b) => {
        val l = exec(a,env)
                val r = exec(b,env)
                op match {
                case "+" => (l,r) match {
                case (l:String,r) => l + r
                case (l,r:String) => l + r
                case (l:Int,r:Int) => l + r
                case (l:Number,r:Number) => l.doubleValue() + r.doubleValue()
                }
                case "-" => (l,r) match {
                case (l:Int,r:Int) => l + r
                case (l:Number,r:Number) => l.doubleValue() - r.doubleValue()
                }
                case "*" => (l,r) match {
                case (l:Int,r:Int) => l * r
                case (l:Number,r:Number) => l.doubleValue() * r.doubleValue()
                case (l:String,r:Long) => l * r.toInt
                }
                case "/" => (l,r) match {
                case (l:Int,r:Int) => l / r
                case (l:Number,r:Number) => l.doubleValue() / r.doubleValue()
                }
                case "%" => (l,r) match {
                case (l:Int,r:Int) => l % r
                case (l:Number,r:Number) => l.doubleValue() % r.doubleValue()
                }
                case "==" => (l,r) match {
                case (l:Number,r:Number) => l == r
                case (l:String,r:String) => l == r
                case (l:Boolean,r:Boolean) => l == r
                }
                case "!=" => (l,r) match {
                case (l:Number,r:Number) => l != r
                case (l:String,r:String) => l != r
                case (l:Boolean,r:Boolean) => l != r
                }
                case "<=" => (l,r) match {
                case (l:Number,r:Number) => l.doubleValue() <= r.doubleValue()
                case (l:String,r:String) => l <= r
                }
                case ">=" => (l,r) match {
                case (l:Number,r:Number) => l.doubleValue() >= r.doubleValue()
                case (l:String,r:String) => l >= r
                }
                case "<" => (l,r) match {
                case (l:Number,r:Number) => l.doubleValue() < r.doubleValue()
                case (l:String,r:String) => l < r
                }
                case ">" => (l,r) match {
                case (l:Number,r:Number) => l.doubleValue() > r.doubleValue()
                case (l:String,r:String) => l > r
                }
                case "&&" => (l,r) match {
                case (l:Boolean,r:Boolean) => l && r
                }
                case "||" => (l,r) match {
                case (l:Boolean,r:Boolean) => l || r
                }
        }
    }
    case UnOp(op,a) => {
        val b = exec(a,env)
                op match {
                case "+" => b match {
                case b:Number => b
                }
                case "-" => b match {
                case b:Number => -b.doubleValue()
                }
                case "!" => b match {
                case b:Boolean => !b
                case n:Number => n != 0
                case f:Func => f(List())
                }
        }
    }
    case IfExpr(a,b,c) => {
        val d = exec(a,env)
                d match {
                case true => exec(b,env)
                case false => exec(c,env)
        }
    }
    case NumberLit(s) => if(s.indexOf(".") == -1) s.toInt else s.toDouble
    case StringLit(s) => s
    case WordLit(s) => env.get(s) match {
    case Some(x) => x
    case None => throw new IllegalStateException(s + " is not found")
    }
    case FuncLit(as,e) => FuncVal(as,e,env)
    case CallOp(a,args) => {
        val f = exec(a,env)
                f match {
                case f:Func => {
                    val vargs = args.map{exec(_,env)}
                    f.apply(vargs)
                }
        }
    }
    case Empty() => ()
    }
}