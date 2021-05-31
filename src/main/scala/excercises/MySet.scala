package excercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {  // extends Function1[A, Boolean]
  override def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}

class MyEmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new MyNonEmptySet(elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new MyEmptySet[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new MyEmptySet[B]
  override def filter(predicate: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()
}

class MyNonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A]{
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)
  override def +(elem: A): MySet[A] = if (contains(elem)) this else new MyNonEmptySet(elem, this)
  /*
    [1,2,3] ++ [4,5] =
      [2,3] ++ [4,5] + 1
      [3] ++ [4,5] + 1 + 2
      [] ++ [4,5] + 1 + 2 + 3
      [4,5] + 1 + 2 + 3 = [4,5,1,2,3]
   */
  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head
  /*
    [1,2,3].map(_*2) =
      [2,3].map(_*2) + 1*2
      [3].map(_*2) + 1*2 + 2*2
      [].map(_*2) + 1*2 + 2*2 + 3*2
      [] + 1*2 + 2*2 + 3*2 = [2,4,6]
   */
  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)  // can't be f(head) + tail.map(f) as + has to be of method of MySet
  /*
    [1,2,3].flatMap(x=>List(x,x*10)) =
      [2,3].flatmap(...) ++ [1,10]
      [3].flatmap(...) ++ [1,10] ++ [2, 20]
      [].flatmap(...) ++ [1,10] ++ [2, 20] ++ [3, 30]
      [] ++ [1,10] ++ [2, 20] ++ [3, 30] = [1,10,2,20,3,30]
   */
  override def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)

  /*
    [1,2,3].filer(_ % 2 != 0) =
      [2,3].filter(...) + 1
      [3].filter(...)
      [].filter(...)  + 1 + 3
      [] + 1 + 3 = [1,2,3]

   */
  override def filter(predicate: A => Boolean): MySet[A] = {
    val filterTail = tail.filter(predicate)
    if (predicate(head)) filterTail + head else filterTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
}

object MySet {
  /*
    apply(1,2,3)
      buildSet([1,2,3], [])
      buildSet([2,3], [] + 1)
      buildSet([3], [1] + 2)
      buildSet([], [2,1] + 3)
      [3,2,1]
   */
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] ={
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    }
    buildSet(values.toSeq, new MyEmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1, -3) + 3 foreach println  // output 5 4 3 2 1 -3 -1 because + adds to begging and not end
  s + 5 ++ MySet(-1, -3) + 3 map (_ * 10) foreach println  // 50 40 30 20 10 -30 -10
  s + 5 ++ MySet(-1, -3) + 3 flatMap(x => MySet(x, x * 100)) foreach println // -100 -1 -300 -3 100 1 200 2 300 3 400 4 500 5
  s + 5 ++ MySet(-1, -3) + 3 flatMap(x => MySet(x, x * 10)) filter(_ % 2 == 0) foreach println // -10 -30 10 20 2 30 40 4 50
}