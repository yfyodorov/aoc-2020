    sealed trait Token {}

    case class Number(num: Long) extends Token {
      // def this(str: String) = this(str.toInt)
      def *(n: Number) = Number(num*n.num)
      def +(n: Number) = Number(num+n.num)
      override def toString: String = num.toString
    }

    sealed trait Op extends Token {
      def apply(n1: Long, n2: Long): Long
    }

    case class Add() extends Op {
      override def toString: String = "+"
      def apply(n1: Long, n2: Long) = n1 + n2
    }

    case class Multiply() extends Op {
      override def toString: String = "*"
      def apply(n1: Long, n2: Long) = n1 * n2
    }

    case class Start() extends Op {
      override def toString: String = "START"
      def apply(n1: Long, n2: Long) = n2
    }

    case class OpenParen() extends Token {
      override def toString: String = "["
    }

    case class CloseParen() extends Token {
      override def toString: String = "]"
    }


    object AdventOfCode18 {

      val digit = "[0-9]".r
      val ws = "\\s".r

      def pushToken(token: Token, tokens: Seq[Token], curr: String) =
        if (curr.nonEmpty)
          (token +: (Number(curr.toLong) +: tokens), "")
        else
          (token +: tokens, "")

      def endToken(tokens: Seq[Token], curr: String) =
        if (curr.nonEmpty)
          (Number(curr.toInt) +: tokens, "")
        else
          (tokens, "")

      def parse(str: String) = {
        val (tokens, curr) = str.foldLeft((Seq[Token](), "")) {
          case ((tokens, curr), ch) => ch match {
            case '*' => pushToken(Multiply(), tokens, curr)
            case '+' => pushToken(Add(), tokens, curr)
            case '(' => pushToken(OpenParen(), tokens, curr)
            case ')' => pushToken(CloseParen(), tokens, curr)
            case digit() => (tokens, curr + ch)
            case ws() => endToken(tokens, curr)
            case _ => throw new Exception("Parse error at " + ch + "; currently parsed " + tokens)
          }
        }

        endToken(tokens, curr)._1.reverse
      }

      def evaluate(tokens: Seq[Token]): Long = {
        val (res, left) = evaluate_aux(0, Start(), tokens)

        if (left.nonEmpty)
          throw new Exception("Tokens left " + tokens)
        else
          res
      }

      def evaluate_aux(acc: Long, op: Op, tokens: Seq[Token]): (Long, Seq[Token]) = tokens match {
        case Nil => throw new Exception("Token expected after " + op)
        case Number(num) :: Nil =>
          (op(acc, num), Nil)
        case Number(num) :: CloseParen() :: rest =>
          (op( acc, num), rest)
        case Number(num) :: (op2: Op) :: rest =>
          evaluate_aux(op(acc,num), op2, rest)
        case OpenParen() :: rest =>
          val (res, newRest) = evaluate_aux(0, Start(), rest)
          evaluate_aux(acc, op, Number(res) +: newRest)

        case tok :: _ => throw new Exception("Token " + tok + " not allowed after " + op)
      }

      def evaluate2(tokens: Seq[Token]): Long = {
        val (res, left) = evaluate2_aux(Seq(Start()), tokens)

        if (left.nonEmpty)
          throw new Exception("Tokens left " + tokens)

        res match {
          case Number(n) :: Nil => n
          case Number(_) :: _ => throw new Exception("Unsconsumed stack "+res)
          case _ => throw new Exception("bad stack "+res)
        }
      }

      def popAllMuls(stack: Seq[Token]): Seq[Token] = stack match {
          case Number(_) :: Nil => stack
          case (n : Number) :: Start() :: rest => n :: rest
          case Number(n1) :: Multiply() :: Number(n2) :: rest =>
            popAllMuls(Number(n1*n2) :: rest)
          case Number(_) :: Add() :: Number(_) :: _ => stack
          case _ => throw new Exception("Bad stack "+stack)
        }

      def pushStack(stack: Seq[Token], n: Number) = stack match {
        case Add() :: Number(n1) :: rest => Number(n.num+n1) :: rest
        case (_: Multiply | _: Start) :: _ => n +: stack
        case _ => throw new Exception("bad stack "+stack)
      }

      def evaluate2_aux(stack: Seq[Token], tokens: Seq[Token]): (Seq[Token], Seq[Token]) = tokens match {
        case Nil => throw new Exception("Token expected after " + stack.reverse)
        case (n : Number) :: Nil =>
          (popAllMuls(pushStack(stack, n)), Nil)
        case (n : Number) :: CloseParen() :: rest =>
          (popAllMuls(pushStack(stack, n)), rest)
        case (n : Number) :: (op: Op) :: rest =>
          evaluate2_aux(op +: pushStack(stack,n), rest)
        case (_ : Number) :: _ =>
          throw new Exception("operation  or close paren expected at " + tokens)
        case OpenParen() :: rest =>
          val (newStack, left) = evaluate2_aux(Start() +: stack, rest)
          evaluate2_aux(newStack.tail, newStack.head+: left)
        case tok :: _ => throw new Exception("Token " + tok + " not allowed after " + stack)
      }

      def tests() = {
        val parsed = parse("1 + 2*(3+(4*5))")
        //   println(parsed)
        //   println(evaluate(parsed))

        val examples = Seq(
          "1 + 2 * 3 + 4 * 5 + 6" -> 71,
          "1 + (2 * 3) + (4 * (5 + 6))" ->  51,
          "2 * 3 + (4 * 5)" -> 26,
          "5 + (8 * 3 + 9 + 3 * 4 * 3)" -> 437,
          "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" ->  12240,
          "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" ->  13632
        )

        val examples2 = Seq(
          "1 + 2 * 3 + 4 * 5 + 6" -> 231,
          "1 + (2 * 3) + (4 * (5 + 6))" -> 51 ,
          "2 * 3 + (4 * 5)" -> 46,
          "5 + (8 * 3 + 9 + 3 * 4 * 3)" -> 1445,
          "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" ->  669060,
          "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" ->  23340
        )

        for((ex,ans) <- examples) {
          val res = evaluate(parse(ex))
          if(res != ans)
            println("In "+ex+"\nGot "+res+" expected "+ans)
        }

        for((ex,ans) <- examples2) {
          val res = evaluate2(parse(ex))
          if(res != ans)
            println("In "+ex+"\nGot "+res+" expected "+ans)
        }

      }

      def fromFile(file: String) = {
        val res = Source.fromFile(file).getLines().
            map(l => evaluate(parse(l))).sum
        println("Res:"+res)
      }

      def fromFile2(file: String) = {
        val res = Source.fromFile(file).getLines().
            map(l => evaluate2(parse(l))).sum
        println("Res:"+res)
      }
    }
