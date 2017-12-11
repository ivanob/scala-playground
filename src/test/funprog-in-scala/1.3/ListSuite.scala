import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import MyList._

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {

  test("test of the List ADT definition") {
    val l: List[Int] = Cons(3, Cons(2, Cons(1,Nil)))
    assert(l.toString == "Cons(3,Cons(2,Cons(1,Nil)))")
  }

  test("test on basic arithmetic operations on Lists"){
    val l: List[Int] = Cons(3, Cons(2, Cons(1,Nil)))
    assert(List.sum(l) == 6)
    assert(List.mult(l) == 6)
  }

  test("test on apply method"){
    val l: List[Int] = Cons(3, Cons(2, Cons(1,Nil)))
    assert(l == List(3,2,1))
  }

  test("test some basic operations"){
    val l: List[Int] = Cons(1, Cons(2, Cons(3,Nil)))
    val l2: List[Int] = Cons(4, Cons(5, Cons(6,Nil)))
    assert(List.tail(l)==Cons(2, Cons(3,Nil)))
    assert(List.drop(l,2)==Cons(3,Nil))
    assert(List.dropWhile(l)((a:Int)=>(a<2)) == Cons(3,Nil))
    assert(List.setHead(l, 99) == Cons(99, Cons(2, Cons(3,Nil))))
    assert(List.init(l) == Cons(1, Cons(2, Nil)))
    assert(List.sumRight(l) == 6)
    assert(List.multRight(l) == 6)
    assert(List.length(l) == 3)
    assert(List.sumLeft(l) == 6)
    assert(List.multLeft(l) == 6)
    assert(List.lengthLeft(l) == 3)
   // assert(List.reversePlain(l) == Cons(3, Cons(2, Cons(1,Nil))))
    assert(List.reverse(l) == Cons(3, Cons(2, Cons(1,Nil))))
    assert(List.appendRight(l, l2) == List(1,2,3,4,5,6))
    assert(List.concat(List(List(1,2), List(3,4))) == List(1,2,3,4))
    assert(List.addOne(List(1,2,3)) == List(2,3,4))
  }
}