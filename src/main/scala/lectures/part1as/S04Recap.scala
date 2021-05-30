package lectures.part1as

import java.lang
import scala.annotation.tailrec
import scala.util.Try

object S04Recap extends App {
  val aCondition: Boolean = false
  // compiler infers types for us
  val aConditionVal = if (aCondition) 42 else 65 // if expresion returning a value
  // instruction (imperative languages) vs expressions (functional languages)

  val aCodeBlock = { // is the value of the last expresion
    if (aCondition) 54 else 65
    56
  }

  // Unit (void in Java) - used for side-effects, doesn't return a meaningful value
  val theUnit = println("Hello, Scala")
  println(theUnit)

  // functions
  def aFunction(x: Int): Int = x + 1

  // recursion - stack & tail recursion
  def factionalStack(number: Int): Int = if (number <= 1) 1 else number * factionalStack(number - 1)
  def factionalTail(number: Int): Int = {
    @tailrec
    def fact(n: Int, acc: Int): Int = if (n <= 1) acc else fact(n - 1, acc * n)

    fact(number, 1)
  }
  println(s"FactionalStack ${factionalStack(10)}")
  println(s"FactionalTail ${factionalStack(10)}")

  // ** object-oriented programming **
  class Animal

  class Dog extends Animal

  val aDog: Animal = new Dog // OO polymorphism by subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch!")
  }

  // method notations
  val aCrocodile = new Crocodile
  aCrocodile.eat(aDog)
  aCrocodile eat aDog // infix notation
  println(s"1 + 2  = ${1 + 2}")
  println(s"1.+(2) = ${1.+(2)}")

  // OO anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar!")
  }

  // OO generics
  /*
    class Baz[A]  // An invariant class, trait. - Must be of type A
    class Foo[+A] // A covariant class, trait - Allow subtypes but not supertypes
    class Bar[-A] // A contravariant class, trait - Allows supertypes but not subtypes
   */
  abstract class MyList[+A]

  // OO singleton and companion objects
  object MyList

  // OO case class
  case class Person(name: String, age: Int)

  // expresion and try / catch / finally
  //  val throwsException = throw new RuntimeException  // Type is Nothing
  val aPotentialFailure = try {
    throw new lang.RuntimeException
  } catch {
    case e: Exception => s"Caught: $e"
  } finally {
    println("log info")
  }

  // packaging and import

  // ** functional programming **

  // functions are instances of classes with apply methods
  // lambda is an anonymous functions
  val incrementer1: Function1[Int, Int] = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  val incrementer2: Int => Int = new (Int => Int) {
    override def apply(v1: Int): Int = v1 + 1
  }
  val incrementer3: Int => Int = (v1: Int) => v1 + 1
  val incrementer4: Int => Int = _ + 1
  println(s"3 inc ${incrementer1(3)}, ${incrementer2(3)}, ${incrementer3(3)}, ${incrementer4(3)}")

  val adder1: Function2[Int, Int, Int] = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }
  val adder2: (Int, Int) => Int = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }
  val adder3: (Int, Int) => Int = (v1: Int, v2: Int) => v1 + v2
  val adder4: (Int, Int) => Int = _ + _
  println(s"adder(2, 3) ${adder1(2, 3)}, ${adder2(2, 3)}, ${adder3(2, 3)}, ${adder4(2, 3)}")

  // High Order Function (HOF) take a function as a parameter or as it's return type
  println(List(1, 2, 3).map(incrementer1))

  // for comprehension map, flatmap, filter. List(2-a, 2-b, 2-c, 4-a, 4-b, 4-c, 6-a, 6-b, 6-c)
  val pairsA = for {
    num <- List(1, 2, 3, 4, 5, 6) if num % 2 == 0
    chars <- List('a', 'b', 'c')
  } yield  num + "-" + chars
  println(pairsA)
  val pairsB = List(1,2,3, 4,5,6) filter (_ % 2 == 0) flatMap {num =>
    List('a', 'b', 'c') map (char => num + "-" + char)
  }
  println(pairsB)

  // Scala collection: Seq, Array, List, Vector, Map, Tuple
  val aMap = Map("Daniel" -> 789, "Jess" -> 555)

  // Option, Try
  val aNoneOption = Option(null)  // None
  val aSomeOption = Option(2)   // Some(20
  println(aNoneOption + " " + aSomeOption)
  val aSuccessTry = Try(2)    // Success(2)
  val aFailureTry = Try(throw new RuntimeException) // Failure(java.lang.RuntimeException)
  println(aSuccessTry + " " + aFailureTry)

  // pattern matching
  val x = 2
  val order = x match {
    case 1 => x + "st"
    case 2 => x + "nd"
    case 3 => x + "rd"
    case _ => x + "th"
  }
  val bob = Person("Bob", 22)
  val greetings = bob match {
    case Person(n, _) => "Hi " + n
  }
}