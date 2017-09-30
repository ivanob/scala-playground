
class MyHOFunc[A](val l: List[A]){
  import MyHOFunc._

  def myMap(f:A=>A): List[A] = {
    (for{i<-0 to l.length-1} yield f(l(i))).toList
  }
}

object MyHOFunc {
  implicit def collectionToMyHOFunc[A](i: List[A]): MyHOFunc[A] = new MyHOFunc(i)
}
