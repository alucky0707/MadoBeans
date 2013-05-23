package info.alucky.madobeans

trait AST
object AST{
case class StmtList(lines: List[AST]) extends AST
case class DefineStmt(name: AST, expr:AST) extends AST
case class BinOp(op: String,a: AST, b: AST) extends AST
case class UnOp(op: String,a: AST) extends AST
case class IfExpr(a: AST,b: AST,c: AST) extends AST
case class NumberLit(a: String) extends AST
case class StringLit(a: String) extends AST
case class WordLit(a: String) extends AST
case class NilLit() extends AST
case class FuncLit(args: List[AST],expr: AST) extends AST
case class CallOp(func: AST, args: List[AST]) extends AST
case class Empty() extends AST
}