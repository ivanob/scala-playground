import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import MyOption._

@RunWith(classOf[JUnitRunner])
class OptionSuite extends FunSuite {

  test("test of the basic operations function") {
    val s = Some(5)
    val s2 = Some(-1)
    val n = None
    assert(s.map((a:Int) => a.toString) == Some("5"))
    assert(s.getOrElse("No value") == Some(5))
    assert(n.getOrElse("No value") == "No value")
    assert(n.orElse(Some(99)) == Some(99))
    assert(s.orElse(Some(99)) == Some(5))
    val f = (a:Int)=>if(a>0) true else false
    assert(s.filter(f) == Some(5))
    assert(s2.filter(f) == None)
    assert(s.flatMap((a:Int) => Some(a)) == Some(5))
    assert(s2.flatMap((a:Int) => Some(a)) == Some(-1))
    //New implementations
    assert(s.filter2(f) == Some(5))
    assert(s2.filter2(f) == None)
  }

}
