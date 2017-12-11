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

    //Exercise 3.2
    def tail[A](l: List[A]) = l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

    //Exercise 3.4
    def drop[A](l: List[A], n:Int): List[A] = {
      if(n==0) l
      else drop(tail(l), n-1)
    }

    //Exercise 3.5
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => l
      case Cons(x, xs) => if(f(x)) dropWhile(xs)(f) else xs
    }

    //Exercise 3.3
    def setHead[A](l: List[A], newHead: A): List[A] = l match {
      case Nil => Cons(newHead, Nil)
      case Cons(x, xs) => Cons(newHead, xs)
    }

    //Exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x,init(xs))
    }

    def foldRight[A,B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
      case Nil => acc
      case Cons(x, xs) => f(x,foldRight(xs,acc)(f))
    }

    def sumRight(l: List[Int]): Int = {
      foldRight(l, 0)((a:Int, b:Int) => {a+b})
    }

    def multRight(l: List[Int]): Int = {
      foldRight(l, 1)((a:Int, b:Int) => {a*b})
    }

    //Exercise 3.9
    def length[A](l: List[A]): Int = {
      foldRight(l,0)((a:A,b:Int)=>{1+b})
    }

    //Exercise 3.10
    def foldLeft[A,B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(x,acc))(f)
    }

    //Exercise 3.11
    def sumLeft(l: List[Int]): Int = {
      foldLeft(l, 0)((a:Int, b:Int) => {a+b})
    }

    //Exercise 3.11
    def multLeft(l: List[Int]): Int = {
      foldLeft(l, 1)((a:Int, b:Int) => {a*b})
    }

    //Exercise 3.11
    def lengthLeft[A](l: List[A]): Int = {
      foldLeft(l,0)((a:A,b:Int)=>{1+b})
    }

    //Exercise 3.12
    def reversePlain[A](l: List[A]): List[A] = l match{
      case Nil => Nil
      case Cons(x, Nil) => Cons(x, Nil)
      case Cons(x, xs) => Cons(x,reversePlain(xs))
    }

    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, List[A]())((x, xs)=>Cons(x, xs))
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = {

    }

  }

}