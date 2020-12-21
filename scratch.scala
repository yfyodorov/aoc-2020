  object Aoc1 {
    def run() = {
  
    val nums = Source.fromFile("//Users/yfyodorov/Downloads/aoc1").
        getLines().
        map(_.toInt).
        filter(_ <= 2020).
        toArray.sorted

    val numSet = nums.toSet
    
    for (s <- numSet) {
      if(numSet.contains(2020-s)) println(s*numSet(2020-s))
    }

    for{i <- 0 until nums.length
        j <- i+1 until nums.length
        if nums(i) + nums(j) <= 2020
        k <- j+1 until nums.length
        if nums(i) + nums(j) + nums(k) == 2020} {
        println(nums(i),nums(j),nums(k))
        println(nums(i)*nums(j)*nums(k))
      }

}

object Aoc2 {
    def isValid(line: String) = {
      val pat = raw"(\d+)-(\d+) (\w): (\w+)".r
      line match {
        case pat(from,to,ch,pass) =>
          println(from+","+to+","+ch+","+pass)
          val n = pass.filter(_ == ch(0)).length
          println(n)
          n >= from.toInt && n <= to.toInt
        case _ => throw new Exception("cannot parse "+line)
      }
    }


    def isValid2(line: String) = {
      val pat = raw"(\d+)-(\d+) (\w): (\w+)".r
      line match {
        case pat(froms,tos,chs,pass) =>
          val ch = chs(0)
          val from = froms.toInt-1
          val to = tos.toInt-1

          (pass(from) == ch || pass(to) == ch) && pass(from) != pass(to)
        case _ => throw new Exception("cannot parse "+line)
      }
    }

    def run() {
      println(isValid("1-3 a: abcde"))
      println(isValid("1-3 b: cdefg"))

      println(Source.fromFile("//Users/yfyodorov/Downloads/aoc2").getLines().
          filter(isValid2).length)
     }
  }
        
        
   
