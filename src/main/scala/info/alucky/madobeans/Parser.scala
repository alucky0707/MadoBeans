package info.alucky.madobeans

import scala.language.{postfixOps, higherKinds, implicitConversions}
import scala.util.parsing.combinator._

class Parser extends RegexParsers {
  class ExParser[A](p: Parser[A]) extends Parser[A] {
    def apply(in: Input) = p(in)
    
    def ~~[B](q: => Parser[B]): Parser[~[A,B]] = {
       p~opt("""\s+""".r)~q^^{
         case a~_~b => new ~(a,b)
       }
    }
    def ~~>[B](q: => Parser[B]): Parser[B] = {
      p~>opt("""\s+""".r)~>q
    }
    def <~~[B](q: Parser[B]): Parser[A] = {
      p<~opt("""\s+""".r)<~q
    }
  }
  
  
  implicit def exparser(p:String): ExParser[String] = new ExParser(literal(p))
  implicit def exparser[A](p: Parser[A]): ExParser[A] = new ExParser(p)
  
  
  override protected val whiteSpace = """[ ]+""".r
  
  def program: Parser[AST] = statementList
  def statementList: Parser[AST] = opt("""\s+""".r)~>repsep(statement, """\s+""".r)^^{
    case stmts => AST.StmtList(stmts)
  }
  def statement: Parser[AST] = defineStatement
  def defineStatement: Parser[AST] = opt(wordToken<~":=")~~expression^^{
    case Some(word)~expr => AST.DefineStmt(word,expr)
    case None~expr => expr
  }
  def expression: Parser[AST] = ifExpression
  def ifExpression: Parser[AST] = assignExpression~opt("?"~~>expression~~":"~~expression)^^{
    case a~Some(b~_~c) => AST.IfExpr(a,b,c)
    case a~None => a
  }
  def assignExpression: Parser[AST] = opt(wordToken<~~"=")~~orExpression^^{
    case Some(word)~expr => AST.BinOp("=",word,expr)
    case None~expr => expr
  }
  def orExpression: Parser[AST] = andExpression~rep("||"~~andExpression)^^{
    case left~rest => {
      var ast = left
      rest.foreach{
        case "||"~right => {ast = AST.BinOp("||", ast, right)}
      }
      ast
    }
  }
  def andExpression: Parser[AST] = compareExpression~rep("&&"~~compareExpression)^^{
  case left~rest => {
    var ast = left
      rest.foreach{
        case "&&"~right => {ast = AST.BinOp("&&", ast, right)}
      }
      ast
    }
  }
  def compareExpression: Parser[AST] = addExpression~opt(
                                        ("==="|"!=="|"<="|">="|"<"|">"|"=="|"!=")~~addExpression)^^{
    case left~rest => {
      var ast = left
      rest.foreach{
        case op~right => {ast = AST.BinOp(op, ast, right)}
      }
      ast
    }
  }
  def addExpression: Parser[AST] = mulExpression~rep(("+"|"-")~~mulExpression)^^{
    case left~rest => {
      var ast = left
      rest.foreach{
        case op~right => {ast = AST.BinOp(op, ast, right)}
      }
      ast
    }
  }
  def mulExpression: Parser[AST] = powExpression~rep(("*"|"/"|"%")~~powExpression)^^{
    case left~rest => {
      var ast = left
      rest.foreach{
        case op~right => {ast = AST.BinOp(op, ast, right)}
      }
      ast
    }
  }
  def powExpression: Parser[AST] = unaryExpression~opt("^"~~>unaryExpression)^^{
    case left~Some(right) => AST.BinOp("^",left,right)
    case left~None => left
  }
  def unaryExpression: Parser[AST] = opt("+"|"-"|"!")~~primary^^{
    case Some(op)~v => AST.UnOp(op,v)
    case None~v => v
  }
  def primary: Parser[AST] = opt((wordToken|
                             numberLiteral|
                             stringLiteral|
                             functionLiteral|
                             "("~~>statementList<~~")")~rep(functionCall))^^{
    case None => AST.Empty()
    case Some(v~rest) => {
      var ast = v
      rest.foreach{
        case args => {ast = AST.CallOp(ast,args)}
      }
      ast
    }
  }
  def numberLiteral: Parser[AST] = """[0-9]+(\.[0-9]*)?""".r^^{
    case s => AST.NumberLit(s)
  }
  def stringLiteral: Parser[AST] = "\""~>"""([^\"]*)""".r<~"\""^^{
    case s => AST.StringLit(s)
  }
  def functionLiteral: Parser[AST] = "{"~>repsep(wordToken,",")~"->"~~statementList<~~"}"^^{
    case as~_~stmts => AST.FuncLit(as,stmts)
  }
  def functionCall: Parser[List[AST]] = "("~~>repsep(expression, ",")<~~")"
  def wordToken: Parser[AST] = """[a-zA-Z_]+""".r^^{
    case str => AST.WordLit(str)
  }
  
  def parse(str:String): Option[AST] = parseAll(program, str) match {
    case Success (result, _) => Some(result)
    case failure: NoSuccess => System.err.println(failure);None
  }
}