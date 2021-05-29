package lectures.part1as

object AdvancedPatternMatching extends App {

  /****
  2.6. Advanced Pattern Matching, Part 1
  ****/
  class Person(val name: String, val age: Int)
  object Person{  // best practise is have the unapply in the Companion object
    def unapply(person: Person): Option[(String, Int)] = if (person.age < 21) None else Option(person.name, person.age)
    def unapply(age: Int): Option[String] = Option(if (age < 21) "minor" else "major")
  }
  object TwinPerson {  // unapply doesn't have to be in the Companion Object
    def unapply(person: Person): Option[((String, Int),(String, Int))] =
      Option((person.name + 1, person.age + 1), (person.name + 2, person.age + 2))

    def unapply(persons: (Person, Person)): Option[((String, Int),(String, Int))] =
      Option((persons._1.name, persons._1.age), (persons._2.name, persons._2.age))
  }

  val bob = new Person("Bob", 20)
  val sue = new Person("Sue", 25)
  val greetings = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"  // uses def unapply(person: Person): Option[(String, Int)]
    case TwinPerson((p1, a1), (p2, a2)) => s"Greetings $p1 = $a1, $p2 = $a2" // uses def unapply(person: Person): Option[((String, Int),(String, Int))]
  }
  println(greetings)  // 1st unapply returns None but 2nd returns Some(). Output:  Greetings Bob1 = 21, Bob2 = 22

  val hi = (bob, sue) match {
    case TwinPerson((p1, a1), (p2, a2)) => s"Hi $p1 = $a1, $p2 = $a2"  // uses def unapply(persons: (Person, Person)): Option[((String, Int),(String, Int))]
  }
  println(hi) // Output: Hi Bob = 20, Sue = 25

  val legalStatus = bob.age match {
    case Person(status) => s"I am a $status"  // uses def unapply(age: Int): Option[String]
  }
  println(legalStatus)  // Output: I am a minor

  /*
    Exercise - Using unapply for conditions
  */
  object even {  // Usefully use lowercase name if just using object just to unapply conditions. Also no need for Option[]
    def unapply(number: Int): Boolean = number % 2 == 0
  }
  object singleDigit {
    def unapply(number: Int): Boolean = number > -10 && number < 10
  }
  val n: Int = 44
  val mathProperty = n match {
    case singleDigit() => s"$n single digit"
    case even() => s"$n an even number"
    case _ => "no property"
  }
  println(mathProperty) // output: 44 an even number

  /****
  2.7. Advanced Pattern Matching, Part 2
  ****/
  // infix pattern
  class Or[A, B](val a: A, val b: B) // Like Scala Either class
  object Or {
    def unapply[A, B](or: Or[A, B]): Option[(A, B)] = Option(or.a, or.b)
  }
  val or = new Or(2, "two")
  val humanDescription = or match {
   // case Or(num, str) => s"$num is written as $str"
    case num Or str => s"$num is written as $str" // infix, same as case Or(num, str)
  }
  println(humanDescription)

  val numbers = List(1)
  val description = numbers match {
    //   case h :: Nil => s"The only element is $h"  // infix
    case ::(h, Nil) => s"The only element is $h" // non infix
    case _ => ""
  }
  println(description)

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starts with 1"
  }
  println(vararg)

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object MyEmpty extends MyList[Nothing]
  case class MyCons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]
  object MyList{
    def unapplySeq[A](myList: MyList[A]): Option[Seq[A]] = { // takes a MyList[A] and returns a Option[Seq[A]]
      if (myList == MyEmpty) Some(Seq.empty)
      else unapplySeq(myList.tail).map(x => myList.head +: x)
    }
  }
  val myList: MyList[Int] = MyCons(1, MyCons(2, MyCons(3, MyEmpty)))
  val decomposed = myList match {
    case myL @ MyList(1, 2, _*) => s"Starts 1 and 2. $myL" // Works because unapply returns a sequence which is then
    case _ => "Something else"   // matched with 1, 2. Because of _* compiler looks for unapplySeq instead of unapply
  }
  println(decomposed) // Output: Starts 1 and 2. MyCons(1,MyCons(2,MyCons(3,MyEmpty)))

  // Custom return types for unapply.
  // isEmpty: Boolean, get: something  (replicating Option.isEmpty and Option.get)
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }
  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false
      override def get: String = person.name.toUpperCase
    }
  }
  val person = bob match {
    case PersonWrapper(name) => s"Unwrapped name $name"
  }
  println(person) // output: Unwrapped name BOB

}
