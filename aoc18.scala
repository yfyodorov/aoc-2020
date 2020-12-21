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

    val finished =
      if (curr.nonEmpty)
        endToken(tokens, curr)._1
      else
        tokens

    finished.reverse
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
    case tok :: rest =>
     // println("acc="+acc+" op="+op+" tokens="+tokens)
      tok match {
        case Number(num) =>
          val newAcc = op match {
            case _: Start => num
            case _: Add => acc + num
            case _: Multiply => acc * num
          }
          if (rest.isEmpty || rest.head.isInstanceOf[CloseParen])
            (newAcc, rest)
          else if (rest.head.isInstanceOf[Op])
            evaluate_aux(newAcc, rest.head.asInstanceOf[Op], rest.tail)
          else
            throw new Exception("operation expected at "+tokens)
        case _: OpenParen =>
          val (res, left) = evaluate_aux(0, Start(), rest)
          if (!left.head.isInstanceOf[CloseParen])
            throw new Exception("Missing ) at "+left)
          else {
            evaluate_aux(acc, op, Number(res) +: left.tail)
          }
        case _ => throw new Exception("Token " + tok + " not allowed after " + op)
      }
  }

  def evaluate2(tokens: Seq[Token]): Long = {
    val (res, left) = evaluate2_aux(Seq(Start()), tokens)

    if (left.nonEmpty)
      throw new Exception("Tokens left " + tokens)
    else {
      val res1 = popAllMuls(res)

      if (!res1.tail.head.isInstanceOf[Start])
        throw new Exception("Unsconsumed stack "+res1)
      else if(!res1.head.isInstanceOf[Number])
        throw new Exception("bad stack "+res1)
      else
          res1.head.asInstanceOf[Number].num
    }
  }

  def popAllMuls(stack: Seq[Token]): Seq[Token] = {
  //  println("reducing: "+stack)
    stack match {
    case Number(n1) :: Multiply() :: Number(n2) :: rest =>
      popAllMuls(Number(n1*n2) :: rest)
    case Number(_) :: Start() :: _ => stack
    case Number(_) :: Add() :: Number(_) :: _ => stack
    case _ => throw new Exception("Bad stack "+stack)
  }
  }

  def popAdd(num: Number, stack: Seq[Token]) = stack match {
    case Add() :: Number(n) :: rest => Number(num.num + n) :: rest
    case _ => throw new Exception("bad stack "+stack)
  }

  def evaluate2_aux(stack: Seq[Token], tokens: Seq[Token]): (Seq[Token], Seq[Token]) = tokens match {
    case Nil => throw new Exception("Token expected after " + stack.reverse)
    case tok :: rest =>
   //   println("stack="+stack+" <=> " + " tokens="+tokens)
      tok match {
        case Number(num) =>
            val newStack = stack.head match {
              case _: Add => popAdd(Number(num), stack)
              case _: Multiply => tok +: stack
              case _: Start => tok +: stack
            }
            if (rest.isEmpty || rest.head.isInstanceOf[CloseParen])
              (popAllMuls(newStack), rest)
            else if (rest.head.isInstanceOf[Op])
              evaluate2_aux(rest.head +: newStack, rest.tail)
            else
              throw new Exception("operation  or close paren expected at " + tokens)
        case _: OpenParen =>
          val (newStack, left) = evaluate2_aux(Start() +: stack, rest)
          if (!left.head.isInstanceOf[CloseParen])
            throw new Exception("Missing ) at "+left)
          else if(!newStack.tail.head.isInstanceOf[Start])
            throw new Exception("bad stack after close paren "+newStack+" at "+left)
          else {
            evaluate2_aux(newStack.tail.tail, newStack.head+: left.tail)
          }
        case _ => throw new Exception("Token " + tok + " not allowed after " + stack)
      }
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
