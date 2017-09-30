/**
  * This class contains my own implementation of the high order functions
  * available in scala. It is coded for educational purposes so maybe they
  * are not the most efficient implementations.
  */
class MyHOFunc[A](val l: List[A]){
  import MyHOFunc._

  def myMap(f:A=>A): List[A] = {
    (for{i<-0 to l.length-1} yield f(l(i))).toList
  }

  def myZip[B](l2:List[B]):List[(A,B)] = {
    (for { i<-0 to l.length-1 if(l2.length>i)} yield (l(i),l2(i))).toList
  }

  def myFind(f:A => Boolean): Option[A] = {
    (for{
      i<- 0 to l.length-1
      if(f(l(i)))}
      yield l(i)).toList
        match {
         case x::xs => Some(x)
         case Nil => None
      }
  }

  def myReduce(f:(A,A)=>A): A = l match {
    case x :: Nil => x
    case x :: xs => f(x,xs.myReduce(f))
    case Nil => throw new Exception("ERROR")
  }

}

object MyHOFunc {
  implicit def collectionToMyHOFunc[A](i: List[A]): MyHOFunc[A] = new MyHOFunc(i)
}
