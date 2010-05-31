package t2ast

object AST {

	abstract sealed class TypeDescriptor
		case class PrimitiveType(kind: PrimitiveTypeKind) extends TypeDescriptor
		case class ReferenceType(name: String, qualified: Boolean) extends TypeDescriptor
		case class ParameterizedType(component: TypeDescriptor, params: List[TypeDescriptor]) extends TypeDescriptor
		case class ArrayType(component: TypeDescriptor) extends TypeDescriptor

	abstract sealed class PrimitiveTypeKind
		case object KByte extends PrimitiveTypeKind
		case object KShort extends PrimitiveTypeKind
		case object KInt extends PrimitiveTypeKind
		case object KLong extends PrimitiveTypeKind
		case object KChar extends PrimitiveTypeKind
		case object KFloat extends PrimitiveTypeKind
		case object KDouble extends PrimitiveTypeKind
		case object KBoolean extends PrimitiveTypeKind
		case object KVoid extends PrimitiveTypeKind


	case class Position(line: Int, column: Int)
	abstract sealed class Node{ def pos: Position }
		case class TypeNode(pos: Position, desc: TypeDescriptor) extends Node
		case class Argument(pos: Position, name: String, typeRef: TypeNode) extends Node

		abstract sealed class Expression extends Node
			case class IntegerLiteral(pos: Position, value: Int) extends Expression
			case class Id(pos: Position, name: String) extends Expression
			case class NullLiteral(pos: Position) extends Expression
			case class BooleanLiteral(pos: Position, value: Boolean) extends Expression

			case class Cast(pos: Position, src: Expression, to: TypeNode) extends Expression
			case class CharacterLiteral(pos: Position, value: Char) extends Expression
			//case class ClosureExpression(pos: Position, typeRef: TypeNode, args: Argument, returns: TypeNode) extends Expression
			case class CurrentInstance(pos: Position) extends Expression
			case class DoubleLiteral(pos: Position, value: Double) extends Expression
			case class FloatLiteral(pos: Position, value: Float) extends Expression
			case class IsInstance(pos: Position, typeRef: TypeNode) extends Expression
			case class ListLiteral(pos: Position, elements: List[Expression]) extends Expression
			case class LongLiteral(pos: Position, value: Long) extends Expression
			case class MemberSelection(pos: Position, target: Expression, name: String) extends Expression
			case class MethodCall(pos: Position, target: Expression, name: String, args: List[Expression]) extends Expression
			case class NewArray(pos: Position, typeRef: TypeNode, args: List[Expression]) extends Expression
			case class NewObject(pos: Position, typeRef: TypeNode, args: List[Expression]) extends Expression
			case class UnqualifiedFieldReference(pos: Position, name: String) extends Expression
			case class UnqualifiedMethodCall(pos: Position, name: String, args: Expression) extends Expression
			case class StaticIDExpression(pos: Position, typeRef: TypeNode, name: String) extends Expression
			case class StaticMethodCall(pos: Position, typeRef: TypeNode, name: String, args: List[Expression]) extends Expression
			case class StringLiteral(pos: Position, value: String) extends Expression
			case class SuperMethodCall(pos: Position, name: String, args: List[Expression]) extends Expression


			abstract sealed class BinaryExpression(symbol: String) extends Expression {
				def left: Expression
				def right: Expression
			}
				case class Addition(pos: Position, left: Expression, right: Expression) extends BinaryExpression("+")
				case class Division(pos: Position, left: Expression, right: Expression) extends BinaryExpression("/")
				case class Multiplication(pos: Position, left: Expression, right: Expression) extends BinaryExpression("*")
				case class Subtraction(pos: Position, left: Expression, right: Expression) extends BinaryExpression("-")

				case class AdditionAssignment(pos: Position, left: Expression, right: Expression) extends BinaryExpression("+=")
				case class Assignment(pos: Position, left: Expression, right: Expression) extends BinaryExpression("=")
				case class BitAnd(pos: Position, left: Expression, right: Expression) extends BinaryExpression("&")
				case class BitOr(pos: Position, left: Expression, right: Expression) extends BinaryExpression("|")
				case class DivisionAssignment(pos: Position, left: Expression, right: Expression) extends BinaryExpression("/=")
				case class Elvis(pos: Position, left: Expression, right: Expression) extends BinaryExpression(":?")
				case class Equal(pos: Position, left: Expression, right: Expression) extends BinaryExpression("==")
				case class GreaterOrEqual(pos: Position, left: Expression, right: Expression) extends BinaryExpression(">=")
				case class GreaterThan(pos: Position, left: Expression, right: Expression) extends BinaryExpression(">")
				case class Indexing(pos: Position, left: Expression, right: Expression) extends BinaryExpression("[]")
				case class LessOrEqual(pos: Position, left: Expression, right: Expression) extends BinaryExpression("<=")
				case class LessThan(pos: Position, left: Expression, right: Expression) extends BinaryExpression("<")  
				case class LogicalAnd(pos: Position, left: Expression, right: Expression) extends BinaryExpression("&&")
				case class LogicalOr(pos: Position, left: Expression, right: Expression) extends BinaryExpression("||")
				case class LogicalRightShift(pos: Position, left: Expression, right: Expression) extends BinaryExpression(">>>")
				case class MathLeftShift(pos: Position, left: Expression, right: Expression) extends BinaryExpression("<<")
				case class MathRightShift(pos: Position, left: Expression, right: Expression) extends BinaryExpression(">>")
				case class Modulo(pos: Position, left: Expression, right: Expression) extends BinaryExpression("%")
				case class ModuloAssignment(pos: Position, left: Expression, right: Expression) extends BinaryExpression("%=")
				case class MultiplicationAssignment(pos: Position, left: Expression, right: Expression) extends BinaryExpression("*=")
				case class NotEqual(pos: Position, left: Expression, right: Expression) extends BinaryExpression("!=")
				case class SubtractionAssignment(pos: Position, left: Expression, right: Expression) extends BinaryExpression("-=")
				case class XOR(pos: Position, left: Expression, right: Expression) extends BinaryExpression("^")
				case class ReferenceEqual(pos: Position, left: Expression, right: Expression) extends BinaryExpression("===")
				case class ReferenceNotEqual(pos: Position, left: Expression, right: Expression) extends BinaryExpression("!==")

			abstract sealed class UnaryExpression(symbol: String) extends Expression {
				def target: Expression
			}
				case class Negate(pos: Position, target: Expression) extends UnaryExpression("-")
				case class Not(pos: Position, target: Expression) extends UnaryExpression("!")
				case class Posit(pos: Position, target: Expression) extends UnaryExpression("+")
				case class PostDecrement(pos: Position, target: Expression) extends UnaryExpression("--")
				case class PostIncrement(pos: Position, target: Expression) extends UnaryExpression("++")


		abstract sealed class Toplevel extends Node

			abstract sealed class Statement extends Toplevel
				case class BlockStatement(pos: Position, elements: List[Statement]) extends Statement
				case class BreakStatement(pos: Position) extends Statement
				case class ContinueStatement(pos: Position) extends Statement
				case class EmptyStatement(pos: Position) extends Statement
				case class IfStatement(pos: Position, condition: Expression, thenBlock: BlockStatement, elseBlock: BlockStatement) extends Statement
				case class ReturnStatement(pos: Position) extends Statement
				case class LocalVariableDeclaration(pos: Position, name: String, typeRef: TypeNode, init: Expression) extends Statement
				case class WhileStatement(pos: Position, condition: Expression, block: BlockStatement) extends Statement
				case class SynchronizedStatement(pos: Position, condition: Expression, block: BlockStatement) extends Statement
				case class ThrowStatement(pos: Position, target: Expression) extends Statement
				case class ForeachStatement(pos: Position, arg: Argument, collection: Expression, statement: BlockStatement) extends Statement
				case class ForStatement(pos: Position, init: Statement, condition: Expression, update: Expression, block: BlockStatement) extends Statement
				case class CondStatement(pos: Position, clauses: List[(Expression, BlockStatement)], elseBlock: Option[BlockStatement]) extends Statement
				// todo elseblock
				case class SelectStatement(pos: Position, condition: Expression, cases: List[(List[Expression], BlockStatement)]) extends Statement
				case class TryStatement(pos: Position, tryBlock: BlockStatement, recClauses: List[(Argument, BlockStatement)], finBlock: Option[BlockStatement]) extends Statement
				// todo finaryblock
				case class ExpressionStatement(pos: Position, body: Expression) extends Statement
}

object main {
	def arg(a:Any):AST.Argument = a match {
	case (l:Int,c:Int,(_,_,a:String),":",t) => AST.Argument(AST.Position(l,c),a,typenode(t))
	}
	def typenode(a:Any):AST.TypeNode = a match {
		case (l:Int,c:Int,"byte") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KByte))
		case (l:Int,c:Int,"short") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KShort))
		case (l:Int,c:Int,"int") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KInt))
		case (l:Int,c:Int,"long") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KLong))
		case (l:Int,c:Int,"char") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KChar))
		case (l:Int,c:Int,"float") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KFloat))
		case (l:Int,c:Int,"double") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KDouble))
		case (l:Int,c:Int,"boolean") => AST.TypeNode(AST.Position(l,c),AST.PrimitiveType(AST.KBoolean))
		case (l:Int,c:Int,a,"[",(_,_,"void"),"]") => val n = typenode(a:Any); AST.TypeNode(AST.Position(l,c),AST.ArrayType(n.desc))
		case (l:Int,c:Int,a:String) => val quarify = true; AST.TypeNode(AST.Position(l,c),AST.ReferenceType(a, quarify))
	}
	def caseblocks(a:Any):List[(AST.Expression, AST.BlockStatement)] = a match {
		case (l:Int,c:Int,"case","(",a,")",b) => List((exp(a),blockstatement(b)))
		case (l:Int,c:Int,a,"@",b) => caseblocks(a):::caseblocks(b)
	}
	def casesblocks(a:Any):List[(List[AST.Expression], AST.BlockStatement)] = a match {
		case (l:Int,c:Int,"case","(",a,")",b) => List((exps(a),blockstatement(b)))
		case (l:Int,c:Int,a,"@",b) => casesblocks(a):::casesblocks(b)
	}
	def exps(a:Any):List[AST.Expression] = a match {
		case (l:Int,c:Int,a,",",b) => exps(a):::exps(b)
		case a => List(exp(a))
	}
	def catchblocks(a:Any):List[(AST.Argument, AST.BlockStatement)] = a match {
		case (l:Int,c:Int,"case","(",a,")",b) => List((arg(a),blockstatement(b)))
		case (l:Int,c:Int,a,"@",b) => catchblocks(a):::catchblocks(b)
	}

	def top(a:Any):AST.Toplevel = statement(a)
	def statement(a:Any):AST.Statement = a match {
		case (l:Int,c:Int,"{",a,"}") => AST.BlockStatement(AST.Position(l,c), statements(a))
		case (l:Int,c:Int,"break") => AST.BreakStatement(AST.Position(l,c))
		case (l:Int,c:Int,"continue") => AST.ContinueStatement(AST.Position(l,c))
		case (l:Int,c:Int,"return") => AST.ReturnStatement(AST.Position(l,c))
		case (l:Int,c:Int,"void") => AST.EmptyStatement(AST.Position(l,c))
		case (l:Int,c:Int,a,";") => statement(a)
		case (l:Int,c:Int,"throw",a) => AST.ThrowStatement(AST.Position(l,c), exp(a))
		case (l:Int,c:Int,"if","(",a,")",(_,_,x,"else",y)) =>
			AST.IfStatement(AST.Position(l,c), exp(a), blockstatement(x), blockstatement(y))
		case (l:Int,c:Int,"if","(",a,")",b) =>
			AST.IfStatement(
				AST.Position(l, c),
				exp(a),
				blockstatement(b),
				AST.BlockStatement(AST.Position(l,c),List(AST.EmptyStatement(AST.Position(l,c))))
			)
		case (l:Int,c:Int,"while","(",a,")",b) =>
			AST.WhileStatement(
				AST.Position(l, c),
				exp(a),
				blockstatement(b))

		case (l:Int,c:Int,(_,_,a:String),":",(_,_,t,"=",x)) => AST.LocalVariableDeclaration(AST.Position(l,c), a, typenode(t), exp(x))
		case (l:Int,c:Int,(_,_,a:String),":",t) => AST.LocalVariableDeclaration(AST.Position(l,c), a, typenode(t), AST.NullLiteral(AST.Position(l,c)))
		case (l:Int,c:Int,"synchronized","(",a,")",b) =>
			AST.SynchronizedStatement(
				AST.Position(l, c),
				exp(a),
				blockstatement(b))
		case (l:Int,c:Int,"foreach","(",(_,_,a,"<-",b),")",x) =>
			AST.ForeachStatement(
				AST.Position(l, c),
				arg(a),
				exp(b),
				blockstatement(x))
		case (l:Int,c:Int,"for","(",a,")",d) =>
			a match {
			case (_,_,a2,"@",(_,_,(_,_,b,";"),"@",x)) =>
				AST.ForStatement(AST.Position(l,c), statement(a2), exp(b), exp(x),blockstatement(d))
			case _ => throw new Exception("error for statements " + a)
			}
		case (l:Int,c:Int,(_,_,"cond"), "{",d,"}") => val cb = caseblocks(d); AST.CondStatement(AST.Position(l,c), cb, None)
		case (l:Int,c:Int,"select","(",a,")", (_,_,"{",d,"}")) => val cb = casesblocks(d); AST.SelectStatement(AST.Position(l,c), exp(a), cb)
		case (l:Int,c:Int,"try","{",a,"}", (_,_,(_,_,"catch"),"{",d,"}")) =>
			val cb = catchblocks(d);
			AST.TryStatement(AST.Position(l,c), blockstatement((l,c,"{",a,"}")), cb, None)

		case a => val e = exp(a); AST.ExpressionStatement(e.pos, e)
//		case _ => throw new Exception("error " + a)
	}

	def blockstatement(a:Any):AST.BlockStatement = a match {
		case (l:Int,c:Int,"{",a,"}") => AST.BlockStatement(AST.Position(l,c), statements(a))
		case _ => throw new Exception("error " + a)
	}

	def statements(a:Any):List[AST.Statement] = a match {
		case (l:Int,c:Int,a,"@",b) => statements(a):::statements(a)
		case a => List(statement(a))
	}

	def exp(a:Any):AST.Expression = a match {
		case (l:Int,c:Int,a:Int) => AST.IntegerLiteral(AST.Position(l,c), a)
		case (l:Int,c:Int,a:Boolean) => AST.BooleanLiteral(AST.Position(l,c), a)
		case (l:Int,c:Int,a:Char) => AST.CharacterLiteral(AST.Position(l,c), a)
		case (l:Int,c:Int,a:Double) => AST.DoubleLiteral(AST.Position(l,c), a)
		case (l:Int,c:Int,a:Float) => AST.DoubleLiteral(AST.Position(l,c), a)
		case (l:Int,c:Int,a:Long) => AST.LongLiteral(AST.Position(l,c), a)
		case (l:Int,c:Int,"this") => AST.CurrentInstance(AST.Position(l,c))
		case (l:Int,c:Int,a:String) => AST.Id(AST.Position(l,c), a)
		case (l:Int,c:Int,"cast","(",t,")",e) => AST.Cast(AST.Position(l,c), exp(e), typenode(t))
		case (l:Int,c:Int,a,"instanceof", b) => AST.IsInstance(AST.Position(l,c), typenode(b))// todo おかしくね？
		case (l:Int,c:Int,(_,_,"List"),"(",a,")") => AST.ListLiteral(AST.Position(l,c), exps(a))
		case (l:Int, c:Int, (_,_,"super"), ".", (_,_,(_,_,b:String),"(",x,")")) => AST.SuperMethodCall(AST.Position(l,c), b,exps(x))
		case (l:Int, c:Int, a, ".", (_,_,(_,_,b:String),"(",x,")")) => AST.MethodCall(AST.Position(l,c), exp(a), b,exps(x))
		case (l:Int, c:Int, a, ".", (_,_,b:String)) => AST.MemberSelection(AST.Position(l,c), exp(a), b)
		case (l:Int, c:Int, "new" ,(_,_,t,"[",a,"]")) => AST.NewArray(AST.Position(l,c), typenode(t), exps(a))
		case (l:Int, c:Int, "new", (_,_,t,"(",a,")"))=> AST.NewObject(AST.Position(l,c), typenode(t), exps(a))
//		case (l:Int, c:Int, ))  //  UnqualifiedFieldReference
//		case (l:Int, c:Int, ))  //  UnqualifiedMethodCall
//		case (l:Int, c:Int, ))  //  StaticIDExpression
//		case (l:Int, c:Int, ))  //  StaticMethodCall
//		case (l:Int, c:Int, ))  //  StringLiteral

		
		case (_,_,_,_,_) => bin(a)
		case (_,_,_,_) => unary(a)
		case _ => throw new Exception("expected expression but found " + a)
	}

	def bin(a:Any):AST.BinaryExpression = a match {
		case (l:Int, c:Int, a, "+", b) => AST.Addition(AST.Position(l,c), exp(a), exp(b))
		case (l:Int, c:Int, a, "-", b) => AST.Division(AST.Position(l,c), exp(a), exp(b))
		case (l:Int, c:Int, a, "*", b) => AST.Multiplication(AST.Position(l,c), exp(a), exp(b))
		case (l:Int, c:Int, a, "/", b) => AST.Subtraction(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "+=", b) => AST.AdditionAssignment(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "=", b) => AST.Assignment(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "&", b) => AST.BitAnd(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "|", b) => AST.BitOr(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "/=", b) => AST.DivisionAssignment(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, ":?", b) => AST.Elvis(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "==", b) => AST.Equal(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, ">=", b) => AST.GreaterOrEqual(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, ">", b) => AST.GreaterThan(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "[]", b) => AST.Indexing(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "<=", b) => AST.LessOrEqual(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "<", b) => AST.LessThan(AST.Position(l,c), exp(a), exp(b))  
        case (l:Int, c:Int, a, "&&", b) => AST.LogicalAnd(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "||", b) => AST.LogicalOr(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, ">>>", b) => AST.LogicalRightShift(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "<<", b) => AST.MathLeftShift(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, ">>", b) => AST.MathRightShift(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "%", b) => AST.Modulo(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "%=", b) => AST.ModuloAssignment(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "*=", b) => AST.MultiplicationAssignment(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "!=", b) => AST.NotEqual(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "-=", b) => AST.SubtractionAssignment(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "^", b) => AST.XOR(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "===", b) => AST.ReferenceEqual(AST.Position(l,c), exp(a), exp(b))
        case (l:Int, c:Int, a, "!==", b) => AST.ReferenceNotEqual(AST.Position(l,c), exp(a), exp(b))

		case _ => throw new Exception("expected binary expression but found " + a)
	}

	def unary(a:Any):AST.UnaryExpression = a match {
		case (l:Int,c:Int,"-",a) => AST.Negate(AST.Position(l,c), exp(a))
		case (l:Int,c:Int,"!",a) => AST.Not(AST.Position(l,c), exp(a))
		case (l:Int,c:Int,"+",a) => AST.Posit(AST.Position(l,c), exp(a))
		case (l:Int,c:Int,"--",a) => AST.PostDecrement(AST.Position(l,c), exp(a))
		case (l:Int,c:Int,"++",a) => AST.PostIncrement(AST.Position(l,c), exp(a))
		case _ => throw new Exception("expected unary expression but found " + a)
	}

	def test(a:Any) {
		val r = exp(p(a))
		println(a + "=>" + r)
	}

	def testtop(a:Any) {
		println(a +"=>")
		val b = p(a)
		println(b +"=>")
		val r = top(b)
		println(""+r)
	}

	def p(a:Any):Any = a match {
		case ("{",a,"}") => (0,0,"{",p(a),"}")
		case ("(",a,")") => (0,0,"(",p(a),")")
		case ("[",a,"]") => (0,0,"[",p(a),"]")
		case (a,"{",b,"}") => (0,0,p(a),"{",p(b),"}")
		case (a,"(",b,")") => (0,0,p(a),"(",p(b),")")
		case (a,"[",b,"]") => (0,0,p(a),"[",p(b),"]")
		case (a,"{",b,"}",c) => (0,0,a,"{",p(b),"}",p(c))
		case (a,"(",b,")",c) => (0,0,a,"(",p(b),")",p(c))
		case (a,"[",b,"]",c) => (0,0,a,"[",p(b),"]",p(c))
		case (a,";") => (0,0,p(a),";")
		case (a,b) =>(0,0,a,p(b))
		case (a,b,c) => (0,0,p(a),b,p(c))
		case a => (0,0,a)
	}

	def main(argv:Array[String]) {

		testtop((1))
		testtop((1,"+",2))
		testtop((1,"+",(2,"*",3)))

		testtop(("{","break","}"))
		testtop(("{",("break","@","break"),"}"))
		testtop((("{",("break","@","break"),"}"),";"))
		testtop((("{",(("break",";"),"@",("break",";")),"}"),";"))
		testtop((("{","continue","}"),";"))
		testtop((("{","void","}"),";"))
		testtop(("if","(",1,")",("{","void","}")))
		testtop(("if","(",1,")",("{","break","}")))
		testtop(("if","(",1,")",(("{","break","}"),"else",("{","break","}"))))
		testtop(("if","(",1,")",(("{","return","}"),"else",("{","return","}"))))
		testtop(("a",":","int"))
		testtop(("a",":",("int","=",1)))
		testtop(("a",":",("int","=",(1,"*",2))))
		testtop(("while","(",1,")",("{","break","}")))

		testtop(("a",":","byte"))
		testtop(("a",":","short"))
		testtop(("a",":","int"))
		testtop(("a",":","long"))
		testtop(("a",":","char"))
		testtop(("a",":","float"))
		testtop(("a",":","double"))
		testtop(("a",":","boolean"))
		testtop(("a",":",("byte","[","void","]")))
		testtop(("a",":",(("byte","[","void","]"),"[","void","]")))
		testtop(("a",":","Test"))
		testtop(("synchronized","(",1,")",("{","void","}")))
		testtop(("throw",1))
		testtop(("foreach","(",(("a",":","int"),"<-",1),")",("{","void","}")))

		val a4 = ((1,";"),"@",((2,";"),"@",3))
		testtop(("for","(", a4, ")",("{",1,"}")))
		testtop(("cond","{",("case","(",1,")",("{",2,"}")),"}" ))
		testtop(("select","(",1,")",("{",("case","(",2,")",("{",3,"}")),"}") ))
		testtop(("try","{","break","}",("catch","{",("case","(",("a",":","int"),")",("{",3,"}")),"}")))
		testtop(true)
		testtop(false)
		testtop(("cast","(","boolean",")",1))
		testtop('a')
		testtop("this")
		testtop(1.5d)
		testtop(1.5f)
		testtop(("1","instanceof", "Int")) // IsInstance
		testtop(("List","(","a",")"))  //  ListLiteral
		testtop((1L))  //  LongLiteral
		testtop(("a",".","b"))  //  MemberSelection
		testtop(("a",".",("b","(",1,")")))  //  MethodCall
		testtop(("new",("Int","[",1,"]")))  //  NewArray
		testtop(("new",("A","(",1,")")))  //  NewObject
//		testtop(())  //  UnqualifiedFieldReference
//		testtop(())  //  UnqualifiedMethodCall
//		testtop(())  //  StaticIDExpression
//		testtop(())  //  StaticMethodCall
//		testtop(())  //  StringLiteral
		testtop(("super",".",("a","(",1,")")))  //  SuperMethodCall

		testtop(("a", "+", "b"))  // Addition
		testtop(("a", "+=", "b"))  // AdditionAssignment
		testtop(("a", "=", "b"))  // Assignment
		testtop(("a", "&", "b"))  // BitAnd
		testtop(("a", "|", "b"))   // BitOr 
		testtop(("a", "/", "b"))   // Division 
		testtop(("a", "/=", "b"))   // DivisionAssignment 
		testtop(("a", ":?", "b"))   // Elvis 
		testtop(("a", "==", "b"))   // Equal 
		testtop(("a", ">=", "b"))   // GreaterOrEqual 
		testtop(("a", ">", "b"))   // GreaterThan 
		testtop(("a", "[]", "b"))   // Indexing 
		testtop(("a", "<=", "b"))   // LessOrEqual 
		testtop(("a", "<", "b"))   // LessThan   
		testtop(("a", "&&", "b"))   // LogicalAnd 
		testtop(("a", "||", "b"))   // LogicalOr 
		testtop(("a", ">>>", "b"))   // LogicalRightShift 
		testtop(("a", "<<", "b"))   // MathLeftShift 
		testtop(("a", ">>", "b"))   // MathRightShift 
		testtop(("a", "%", "b"))   // Modulo 
		testtop(("a", "%=", "b"))   // ModuloAssignment 
		testtop(("a", "*", "b"))   // Multiplication 
		testtop(("a", "*=", "b"))   // MultiplicationAssignment 
		testtop(("a", "!=", "b"))   // NotEqual 
		testtop(("a", "-", "b"))   // Subtraction 
		testtop(("a", "-=", "b"))   // SubtractionAssignment 
		testtop(("a", "^", "b"))   // XOR 
		testtop(("a", "===", "b"))   // ReferenceEqual 
		testtop(("a", "!==", "b"))   // ReferenceNotEqual 

		testtop(("-","a")) // Negate
		testtop(("!","a")) // Not
		testtop(("+","a")) // Posit
		testtop(("--","a")) // PostDecrement
		testtop(("++","a")) // PostIncrement

	}
}
