/**
  * This module contains the exercises from chapter 1.3
  * This is an ADT (Algebraic Data Type) modeling Lists. Is also a functional data structure
  * An ADT is a data type defined with 1 or more constructors, each of these may contain one or
  * more arguments. It is called algebraic because the data structure is the UNION of all the constructors,
  * and each constructor is the PRODUCT of all its arguments.
  */

object MyList {

  trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[A](val head: A, val tail: List[A]) extends List[A]

  object List {

  }

}