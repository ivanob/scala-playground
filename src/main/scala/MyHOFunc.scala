/**
  * This class contains my own implementation of the high order functions
  * available in scala. It is coded for educational purposes so maybe they
  * are not the most efficient implementations.
  */
class MyHOFunc[A](val l: List[A]){
  import MyHOFunc._

  def myMap(f:A=>A): List[A] = {
    (for{elem<- l} yield f(elem))
  }

  def myZip[B](l2:List[B]):List[(A,B)] = {
    (for { i<-0 to l.length-1 if(l2.length>i)} yield (l(i),l2(i))).toList
  }

  def myFind(f:A => Boolean): Option[A] = {
    (for{
      elem <- l
      if(f(elem))}
      yield elem)
        match {
         case x::xs => Some(x)
         case Nil => None
      }
  }

  def myFilter(f:A=>Boolean): List[A] = {
    (for{
      elem <- l
      if(f(elem))}
      yield elem)
  }

  def myReduce(f:(A,A)=>A): A = l match {
    case x :: Nil => x
    case x :: xs => f(x,xs.myReduce(f))
    case Nil => throw new Exception("ERROR")
  }

  def myPartition(f:A=>Boolean): (List[A],List[A]) = l match {
    case x :: xs => {
        val p = xs.myPartition(f)
        if(f(x)) (x::p._1, p._2) else (p._1, x::p._2)
    }
    case Nil => (Nil, Nil)
  }

  def myDrop(i:Int): List[A] = l match {
    case x::xs => if(i==0) x::xs.myDrop(i) else xs.myDrop(i-1)
    case Nil => Nil
  }

  def myDropWhile(f:A=>Boolean): List[A] = l match {
    case x::xs => if(f(x)) xs.myDropWhile(f) else x::xs.myDropWhile(f)
    case Nil => Nil
  }

  /**
    * If we transform the foldLeft operation into a inline expression, we
    * have something like:
    * List(1,2,3,4).myLeftFold(50)(_-_) into:
    * ((((50 - 4) - 3) -2) -1) is equal to 40
    * The accumulator is the MOST LEFT operand
    * Note: FoldLeft CAN be implemented as a tail recursive function
    *
    */
  def myLeftFold[B](init: B)(op:(B,A)=>B):B = {
    def go(acc:B, list:List[A]):B = list match {
      case x :: xs => go(op(acc,x), xs)
      case Nil => acc
    }
    go(init,l)
  }

  /**
    * In this case, we can transform the foldRight operation the inline expression
    * List(1,2,3,4).myRightFold(50)(_-_) into:
    * (1 - (2 - (3 - (4 - 50)))) is equal to 48
    * The accumulator is the MOST RIGHT operand
    * Note: FoldLeft CAN NOT be implemented as a tail recursive function
    */
  def myRightFold[B](init: B)(op:(A,B)=>B):B = {
    def go(acc:B, list:List[A]):B = list match {
      case x :: xs => op(x, go(acc, xs))
      case Nil => acc
    }
    go(init,l)
  }
}

object MyHOFunc {
  implicit def collectionToMyHOFunc[A](i: List[A]): MyHOFunc[A] = new MyHOFunc(i)
}
