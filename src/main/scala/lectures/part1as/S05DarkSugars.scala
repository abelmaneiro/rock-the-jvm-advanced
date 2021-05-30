package lectures.part1as

import scala.util.{Random, Try}

object S05DarkSugars extends App {

//*** #1: methods with single param
  def singleArgMethods(arg: Int) = s"$arg little ducks..."
  val ducks = singleArgMethods {
    // some code
    3 + 2
  }
  val aTryInstance = Try.apply {  // Try.apply
    val value = Random.nextBoolean()
    if (value) throw new RuntimeException else 2
  }
  val listDouble = List(1,2,3) map { x =>
    x * 2
  }

//*** #2: single abstract methods (SAM) trait or abstract class (introduced in Scala 2.12)
  trait Action {
    val action = "ACTION"
    def implemented = "IMPLEMENTED"
    def apply(x: Int): String    // abstract methods
  }
  val actionNew = new Action {
    override def apply(x: Int): String = action + " " + x  // can call member val action
  }
  println(actionNew.apply(0))  // actionNew(0) // outputs: ACTION 0
  val actionSugar1: Action = (x: Int) => "sugar SAM " +  x // can't call member val action, need to include type : Action
  println(actionSugar1.apply(1)) // actionSugar1(1) // outputs: sugar SAM 1

  // passing SAM as a parameter
  class Perform(action: Action) {
    def runAction(n: Int): String = action.apply(n)  //  action(n)
  }
  val performNew = new Perform(new Action {
    override def apply(x: Int): String = action + " perform " + x
  })
  println(performNew.runAction(3))  // ACTION perform 3
  val performSugar = new Perform((x: Int) => "Sugar SAM perform " + x)
  println(performSugar.runAction(4)) // Sugar SAM perform 4

  // example: Runnable
  val aThead = new Thread(new Runnable {
    override def run(): Unit = println("new Runnable")
  })
  aThead.run()  // output: new Runnable
  val aThreadSAM = new Thread(() => println("SAM Runnable"))
  aThreadSAM.run()  // output: SAM Runnable

//*** 3: Methods ending with : are right associative
  val listA = 1 :: 2 :: 3 :: List()   // List(1, 2, 3)
  val listB = List().::(3).::(2).::(1)  // List(1, 2, 3)
  println(listA + " - " + listB)
  class MyStream[T] {
    def -->: (value: T): MyStream[T] = this
  }
  val myStream = 1 -->: 2 -->: 3 -->: 4 -->: new MyStream[Int]
  println(myStream)

//*** 4: multi-word method naming using ` `
  case class TeenGirl(name: String) {
    def `and then said`(gossip: String): String = s"$name said $gossip"
  }
  val lillySaid = TeenGirl("Lilly").`and then said`("Scala is great")
  println(lillySaid)

//*** 5: Generics - infix type (similar to infix method e.g. +(v1: Int, v2: Int)    2.+(3) or 2 + 3
  case class Composite[A, B](v1: A, v2: B) {}
  val compA1 = new Composite[String, Int]("compA", 1)
  val compA2 = new (String Composite Int)("compA", 2)
  val compB1: Composite[String, Int] = new Composite("compB", 1)
  val compB2: String Composite Int = new Composite("compB", 2)
  println(s"$compA1 $compA2 $compB1 $compB2")

  class ---> [A, B]
  val towards1: --->[Int, String] = new --->
  val towards2: Int ---> String = new --->

  trait :<[A, B]
  val lessThan: Int :< Int = ???

//*** 6 update() used in mutable collections (returns Unit)
  val anArray = Array("a","b","c","d","e")
  anArray(2) = "cc"   // Array("a","b","cc","d","e")
  anArray.update(3, "dd")   // Array("a","b","cc","dd","e")
  println(anArray.mkString(","))

//*** 7 setters_=() using in mutable containers (returns Unit)
  class Mutable {
    private var _value: Int = 0   // private for OO encapsulation
    def value: Int = _value   // getter
    def value_=(aValue: Int): Unit = _value = aValue
  }
  val mutable = new Mutable
  mutable.value = 3
  println(mutable.value)
  mutable.value_=(9)
  println(mutable.value)



}
