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

  test("test of DROP function"){
    assert((1 to 10).toList.myDrop(5) == List(6,7,8,9,10))
    assert((1 to 10).toList.myDrop(15) == Nil)
  }

  test("test of DROPWHILE function"){
    assert((1 to 10).toList.myDropWhile(x=>x<4) == List(4,5,6,7,8,9,10))
    assert((1 to 10).toList.myDropWhile(x=>x%2==0) == List(1,3,5,7,9))
  }

  test("test of LEFT_FOLD function"){
    //Commutative operation: +
    assert((1 to 10).toList.myLeftFold(0)((a,b)=>a+b) == 55)
    assert((1 to 10).toList.myLeftFold(5)((a,b)=>a+b) == 60) //With initial value
    //Non-commutative operation: -
    assert(List(1).myLeftFold(10)((a,b)=>a-b) == List(1).foldLeft(10)(_-_)) //res = 9
    assert(List(1).myLeftFold(0)((a,b)=>a-b) == List(1).foldLeft(0)(_-_)) //res = -1
    assert(List(1,2).myLeftFold(0)((a,b)=>a-b) == List(1,2).foldLeft(0)(_-_)) //res = -3
    assert(List(1,2,3).myLeftFold(0)((a,b)=>a-b) == List(1,2,3).foldLeft(0)(_-_)) //res = -6
    assert(List(1,2,3,4).myLeftFold(0)((a,b)=>a-b) == List(1,2,3,4).foldLeft(0)(_-_)) //res = -10
    assert(List(1,2,3,4,5).myLeftFold(0)((a,b)=>a-b) == List(1,2,3,4,5).foldLeft(0)(_-_)) //res = -15
    assert(List(1,2,3,4,5,6).myLeftFold(0)((a,b)=>a-b) == List(1,2,3,4,5,6).foldLeft(0)(_-_)) //res = -21
  }

  test("test of RIGHT_FOLD function"){
    //Commutative operation: +
    assert((1 to 10).toList.myRightFold(0)((a,b)=>a+b) == 55)
    assert((1 to 10).toList.myRightFold(5)((a,b)=>a+b) == 60) //With initial value
    //Non-commutative operation: -
    assert(List(1).myRightFold(10)((a,b)=>a-b) == List(1).foldRight(10)(_-_)) //res = -9
    assert(List(1).myRightFold(0)((a,b)=>a-b) == List(1).foldRight(0)(_-_)) //res = 1
    assert(List(1,2).myRightFold(0)((a,b)=>a-b) == List(1,2).foldRight(0)(_-_)) //res = -1
    assert(List(1,2,3).myRightFold(0)((a,b)=>a-b) == List(1,2,3).foldRight(0)(_-_)) //res = 2
    assert(List(1,2,3,4).myRightFold(0)((a,b)=>a-b) == List(1,2,3,4).foldRight(0)(_-_)) //res = -2
    assert(List(1,2,3,4,5).myRightFold(0)((a,b)=>a-b) == List(1,2,3,4,5).foldRight(0)(_-_)) //res = 3
    assert(List(1,2,3,4,5,6).myRightFold(0)((a,b)=>a-b) == List(1,2,3,4,5,6).foldRight(0)(_-_)) //res = -3
  }

  test("test of LEFT_REDUCE function"){
    assert(List(1).myLeftReduce((a,b)=>a-b) == List(1).reduceLeft(_-_)) //res = 1
    assert(List(1,2).myLeftReduce((a,b)=>a-b) == List(1,2).reduceLeft(_-_)) //res = -1
    assert(List(1,2,3).myLeftReduce((a,b)=>a-b) == List(1,2,3).reduceLeft(_-_)) //res = -4
    assert(List(1,2,3,4).myLeftReduce((a,b)=>a-b) == List(1,2,3,4).reduceLeft(_-_)) //res = -8
  }

  test("test of RIGHT_REDUCE function"){
    assert(List(1).myRightReduce((a,b)=>a-b) == List(1).reduceRight(_-_)) //res = 1
    assert(List(1,2).myRightReduce((a,b)=>a-b) == List(1,2).reduceRight(_-_)) //res = -1
    assert(List(1,2,3).myRightReduce((a,b)=>a-b) == List(1,2,3).reduceRight(_-_)) //res = 2
    assert(List(1,2,3,4).myRightReduce((a,b)=>a-b) == List(1,2,3,4).reduceRight(_-_)) //res = -2
  }
}
