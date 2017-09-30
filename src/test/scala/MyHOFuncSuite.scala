import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import MyHOFunc._

@RunWith(classOf[JUnitRunner])
class MyHOFuncSuite extends FunSuite{

  test("test of the map function"){
    assert(List(1,2,3).myMap(x => x) == List(1,2,3)) //Identity function
    assert(List(1,2,3).myMap(x => x*2) == List(2,4,6))
    assert(List(1,2,3,4,5).myMap(x => x*x) == List(1,4,9,16,25))
  }
}
