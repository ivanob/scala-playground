import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import MyTree._

@RunWith(classOf[JUnitRunner])
class TreeSuite extends FunSuite {

  test("test of the size function") {
    val tree1 = Branch(Leaf(1),Leaf(2))
    val tree2 = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))
    assert(Tree.size(tree1) == 2)
    assert(Tree.size(tree2) == 4)

  }

}
