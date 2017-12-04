import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import MyList._

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {

  test("test of the List ADT definition") {
    val l: List = Cons(3, Cons(2, Cons(1,Nil)))
    
  }
}