/**
  * This module contains the exercises from chapter 1.3
  * This is an ADT (Algebraic Data Type) modeling Lists. Is also a functional data structure
  * An ADT is a data type defined with 1 or more constructors, each of these may contain one or
  * more arguments. It is called algebraic because the data structure is the UNION of all the constructors,
  * and each constructor is the PRODUCT of all its arguments.
  */

object MyList {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[A](val head: A, val tail: List[A]) extends List[A]

  object List {
    def sum(l: List[Int]): Int = l match {
      case Nil => 0
      case Cons(x, y) => x + sum(y)
    }

    def mult(l: List[Int]): Int = l match {
      case Nil => 1
      case Cons(0, y) => 0 //Shortcut to stop multiplying in case we find 0
      case Cons(x, y) => x * mult(y)
    }

    def apply[A](l: A*): List[A] = {
        if(l.isEmpty) Nil
        else Cons(l.head, apply(l.tail: _*))
    }

    def tail[A](l: List[A]) = l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

    def drop[A](l: List[A], n:Int): List[A] = {
      if(n==0) l
      else drop(tail(l), n-1)
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => l
      case Cons(x, xs) => if(f(x)) dropWhile(xs)(f) else xs
    }

    def setHead[A](l: List[A], newHead: A): List[A] = l match {
      case Nil => Cons(newHead, Nil)
      case Cons(x, xs) => Cons(newHead, xs)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x,init(xs))
    }

    def foldRight[A,B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
      case Nil => acc
      case Cons(x, xs) => f(x,foldRight(xs,acc)(f))
    }

    def sum2(l: List[Int]): Int = {
      foldRight(l, 0)((a:Int, b:Int) => {a+b})
    }

    def mult2(l: List[Int]): Int = {
      foldRight(l, 1)((a:Int, b:Int) => {a*b})
    }

    def length[A](l: List[A]): Int = {
      foldRight(l,0)((a:A,b:Int)=>{1+b})
    }

    def foldLeft[A,B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(x,acc))(f)
    }
  }

}