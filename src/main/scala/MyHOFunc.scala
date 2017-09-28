
class MyHOFunc(val i: List[Any]){
  import MyHOFunc._

  def myMap(f: Any => Any): List[Any]= {
    i
  }
}

object MyHOFunc {
  implicit def collectionToMyHOFunc(i: List[Any]): MyHOFunc = new MyHOFunc(i)

}
