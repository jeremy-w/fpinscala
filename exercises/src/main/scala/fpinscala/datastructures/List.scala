// vi: set ts=2 sw=2 :
package fpinscala.datastructures

sealed trait List[+A] { // `List` data type, parameterized on a type, `A`
  override
  def toString(): String = {
    // Implementing this in terms of |foldLeft| just hung the REPL for ages
    // as it tried to do string addition, so switched to |StringBuilder|.
    //
    // Needed working printing to use List.repeated in the REPL to play with
    // crashing foldRight with StackOverflowError vs foldLeft working fine:
    //
    //     List.foldRight(bigList, 0)(_ + _)  // BOOM!
    //     List.foldLeft(bigList, 0)(_ + _)   // No problem!
    val builder = this match {
      case Nil => new StringBuilder("List(")
      case Cons(h, t) =>
        List.foldLeft(this, new StringBuilder("List(" + h.toString))((s, a) =>
          s.append(", " + a.toString))
    }
    builder.append(")").toString()
  }
}
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case m => drop(tail(l), m-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def reverse[A](l: List[A]): List[A] = {
    def go[A](l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(h, t) => go(t, Cons(h, acc))
    }
    go(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    def go[A](l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(h, Nil) => acc
      case Cons(h, t) => go(t, Cons(h, acc))
    }
    reverse(go(l, Nil))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, n) => 1 + n)

  /** Used to test stack safety of methods by building a large list.
   *
   *  Use like:
   *
   *      val bigList = List.repeated(250000, 'a')(Nil)
   */
  def repeated[A](n: Int, a: A)(on: List[A]): List[A] = n match {
    case 0 => on
    case m => repeated(m-1, a)(Cons(a, on))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}
