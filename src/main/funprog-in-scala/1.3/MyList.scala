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

    /**
      It is important to note that to call this function with an empty list as 
      the first parameter we will use List[Int]() instead of Nil. Well, Int or whatever
      type we are using. Here is a good explanation why:
      https://stackoverflow.com/questions/5981850/scala-nil-vs-list
      Basically it is like that because Cons(...) returns a List[Int] and if we
      pass Nil, it will be expecting a List[Nothing] in return, which is incorrect.
      Although Nil and List() are idiomatically equivalent this is an example where
      scala can not infer the correct type. It would work in the opposite case: if
      we are expecting List[Int]() and we return Nil
    */
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

    def appendRight[A](l1: List[A], l2: List[A]): List[A] = {
      foldRight(l1,l2)((x,xs)=>Cons(x,xs))
    }

    def concat[A](l: List[List[A]]): List[A] = {
      foldRight(l, List[A]())((innerList:List[A], acc:List[A])=>appendRight(innerList,acc))
    }

    //Exercise 3.16
    def addOne(l: List[Int]): List[Int] = {
      foldRight(l,List[Int]())((a:Int,b: List[Int])=>Cons(a+1,b))
    }

    //Exercise 3.17
    def double2String(l: List[Double]): List[String] = {
      foldRight(l,List[String]())((a:Double, b:List[String]) => Cons(a.toString, b))
    }

    //Exercise 3.18
    def map[A,B](l: List[A])(f: A=>B):List[B] = {
      foldRight(l,List[B]())((a:A, acc:List[B]) => Cons(f(a), acc))
    }

    //Exercise 3.19
    def filter[A](l:List[A])(f:A=>Boolean): List[A] = {
      foldRight(l,List[A]())((a:A, acc:List[A])=>if(f(a)) Cons(a,acc) else acc)
    }

    //Exercise 3.20
    def flatMap[A,B](l: List[A])(f: A=>List[B]):List[B] = {
      foldRight(l, List[B]())((a:A, acc:List[B]) => appendRight(f(a),acc))
    }

    //Exercise 3.21
    def filterFlat[A](l: List[A])(f: A=>Boolean): List[A] = {
      flatMap(l)((a:A)=>if(f(a)) List(a) else Nil)
    }

    //Exercise 3.22
    def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {
      case (Cons(x,xs),Cons(y,ys)) => Cons(x+y, addLists(xs,ys))
      case (Nil, Cons(y,ys)) => Cons(y,ys)
      case (Cons(x,xs), Nil) => Cons(x,xs)
      case (Nil,Nil) => Nil
    }

    //Exercise 3.23
    def zipWith[A,B,C](l1: List[A], l2:List[B])(f: (A,B)=>C): List[C] = (l1,l2) match {
      case (Cons(x,xs),Cons(y,ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
      case (Nil, Cons(y,ys)) => Nil
      case (Cons(x,xs), Nil) => Nil
      case (Nil,Nil) => Nil
    }

    //Exercise 3.34
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Cons(x, xs), Cons(y, ys)) => {
        if(x==y) if(f(xs,ys)) true else hasSubsequence(xs,sub) else hasSubsequence(xs,sub)
      }
      case (Nil,_) => false
      case (_,Nil) => false
    }

    def f[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Cons(x, xs), Cons(y, ys)) => if(x==y) f(xs,ys) else false
      case (Nil,Nil) => true
      case (Nil,_) => false
      case (_,Nil) => true
    }

  }


}
