import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import MyTree._

@RunWith(classOf[JUnitRunner])
class TreeSuite extends FunSuite {

  test("test of the size function") {
    val tree1 = Branch(Leaf(1),Leaf(2))
    val tree2 = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))
    val tree3 = Branch(Branch(Branch(Branch(Leaf(1),Leaf(1)),Leaf(1)),Leaf(1)),Leaf(1))
    assert(Tree.size(tree1) == 2)
    assert(Tree.size(tree2) == 4)
    assert(Tree.maximum(tree1) == 2)
    assert(Tree.maximum(tree2) == 4)
    assert(Tree.depth(tree1) == 2)
    assert(Tree.depth(tree2) == 3)
    assert(Tree.depth(tree3) == 5)
    assert(Tree.map(tree2)((a:Int) => a.toString) == Branch(Branch(Leaf("1"),Leaf("2")),Branch(Leaf("3"),Leaf("4"))))
    val sum = (a:Int, b:Int)=>a+b
    assert(Tree.fold(tree2,0)(sum)(sum) == 10)
  }

}
