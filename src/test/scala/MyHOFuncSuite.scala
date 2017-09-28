import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import MyHOFunc._

@RunWith(classOf[JUnitRunner])
class MyHOFuncSuite extends FunSuite{

  test("test of the map function"){
    assert(List(1,2,3).myMap(x => x) == List(1,2,3))
  }
}
