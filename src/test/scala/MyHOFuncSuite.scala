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

  test("test of PARTITION function"){
    assert((1 to 10).toList.myPartition(x=>x>5) == (List(6,7,8,9,10),List(1,2,3,4,5)))
    assert((1 to 10).toList.myPartition(x=>x%2==0) == (List(2,4,6,8,10),List(1,3,5,7,9)))
  }

  test("test of FILTER function"){
    assert((1 to 10).toList.myFilter(x=>x%2==0) == List(2,4,6,8,10))
  }
}
