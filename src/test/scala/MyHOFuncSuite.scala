import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import MyHOFunc._

@RunWith(classOf[JUnitRunner])
class MyHOFuncSuite extends FunSuite{

  test("test of the MAP function"){
    assert(List(1,2,3).myMap(x => x) == List(1,2,3)) //Identity function
    assert(List(1,2,3).myMap(x => x*2) == List(2,4,6))
    assert(List(1,2,3,4,5).myMap(x => x*x) == List(1,4,9,16,25))
  }

  test("test of ZIP function"){
    assert(List(1,2,3).myZip(List('a,'b,'c)) == List((1,'a),(2,'b),(3,'c)))
    assert(List(1,2,3).myZip(List('a)) == List((1,'a))) //Unpair
    assert(List(1).myZip(List('a,'b,'c)) == List((1,'a))) //Unpair
  }

  test("test of FIND function"){
    assert(List(1,2,3,4,5).myFind(x=>x==1) == Some(1))
    assert(List(1,2,3,4,5).myFind(x=>x>=3) == Some(3))
    assert(List(1,2,3,4,5).myFind(x=>x>10) == None)
  }

  test("test of REDUCE function"){
    assert(List(1).myReduce((a,b)=>a+b) == 1)
    assert(List(1,2).myReduce((a,b)=>a+b) == 3)
    assert(List(1,2,3).myReduce((a,b)=>a+b) == 6)
  }
}
