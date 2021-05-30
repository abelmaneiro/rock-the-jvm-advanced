package lectures.part2afp

import scala.io.Source

object S09PartialFunctions extends App {

  val doubleFunc: Function1[Int, Int] = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 * 2
  }
  val doubleFuncSAM: Function1[Int, Int] = (v1: Int) => v1 * 2   // Single Abstract Method
  val doubleFuncSugarA: Int => Int = (x : Int) => x * 2     // Fully spec
  val doubleFuncSugarB = (x : Int) => x * 2                 // Type inferred
  val doubleFuncSugarC: Int => Int = x => x * 2             // Parameter type inferred
  println(s"Double 5 = ${doubleFunc(5)} ${doubleFuncSAM(5)} ${doubleFuncSugarA(5)} ${doubleFuncSugarB(5)} ${doubleFuncSugarC(5)}")

  val fussyFunctionA = (x: Int) => x match {  // Int => String
    case 1 => "eleven"
    case 2 => "twenty-two"
    case 3 => "thirty-three"
  }
  val fussyFunctionB: Int => String = {  // Int => String
    case 1 => "eleven"
    case 2 => "twenty-two"
    case 3 => "thirty-three"
  }
  println(s"FussFunction  ${fussyFunctionA(2)} ${fussyFunctionB(2)}")

  // Partial functions act on a subset of possible values. E.g. Only Int values of 1,2,3
  val aPartialFunction: PartialFunction[Int, String] = new PartialFunction[Int, String] { // PartialFunction[Int, String]
    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2 || x == 3
    override def apply(v1: Int): String = {
        val array = Array[String]("eleven", "twenty-two", "thirty-three")
        array(v1 - 1)
    }
  }
  val aPartialFunctionSugar: PartialFunction[Int, String] = {
    case 1 => "eleven"
    case 2 => "twenty-two"
    case 3 => "thirty-three"
  }
  println(s"isDefinedAt 2 ${aPartialFunctionSugar.isDefinedAt(2)} ${aPartialFunctionSugar.isDefinedAt(2)}") // Output: true
  println(s"Partial func 2  ${aPartialFunction.apply(2)} ${aPartialFunctionSugar(2)}")  // Output: twenty-two

  // Partial functions are a subset of a "proper/total" function. Can only take ONE parameter type
  // i.e. can be used in-place of "proper" function, but not vice-versa
  println(List(1,2,3) map aPartialFunctionSugar) // Output List(eleven, twenty-two, thirty-three)
  println(List(1,2,3) map fussyFunctionA) // Output List(eleven, twenty-two, thirty-three)
  println(List(0,1,2,3,4) collect aPartialFunctionSugar)  // List(eleven, twenty-two, thirty-three)
  // println(List(0,1,2,3,4) collect fussyFunctionA)  // Compiler error as collect requires PartialFunction

  // passing PartialFunction Anonymously
  val anonymous = List (0,1,2,3,4).collect { // .collect takes a Partial Function
    case 1 => "eleven"
    case 2 => "twenty-two"
    case 3 => "thirty-three"
  }
  println(List(1,2,3).map {   // .map takes Proper Function. Notice missing (x: Int) => x match
    case 1 => "First"
    case 2 => "Second"
    case 3 => "Third"
  })  // Output: List(First, Second, Third)

  val lifted = aPartialFunctionSugar.lift  // Tuns PF into a proper function of Int => Option[String]
  println(s"lift 2 & 0 === ${lifted(2)} & ${aPartialFunction.lift(0)}") // Output: lift 2 & 0 === Some(twenty-two) & None
  val orElse = aPartialFunctionSugar.orElse[Int, String] {  // .orElse[Int,String]  needed else compile error
    case 5 => "fifty-five"
  }
  println(s"orElse 2 & 5  ${orElse(2)} & ${orElse(5)}") // Output: orElse 2 & 5  twenty-two & fifty-five

  /**
   * Exercises
   */

  val manualFussyFunction = new PartialFunction[Int, Int] {
    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2 || x == 5
    override def apply(v1: Int): Int = v1 match {
      case 1 => 42
      case 2 => 65
      case 5 => 999
    }
  }
  println(manualFussyFunction(2))

  val chatterbox: PartialFunction[String, String] = {
    case "hi" => "Hi, may name is Chatterbox"
    case "bye" => "See, you later"
  }
 // Source.stdin.getLines().foreach(line => println(chatterbox(line)))
  Source.stdin.getLines().map(chatterbox).foreach(println)

}
